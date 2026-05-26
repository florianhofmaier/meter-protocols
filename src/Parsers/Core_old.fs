module Mbus.BaseParsers.Core

open System

type PContext =
    | Root
    | Node of name: string * parent: PContext

module PContext =
    let rec toList ctx =
        match ctx with
        | Root -> []
        | Node(name, parent) ->
            name :: toList parent

    let toDisplayPath ctx =
        ctx
        |> toList
        |> List.rev
        |> String.concat "."

[<Struct>]
type SourceSpan =
    {
        Offset : int
        Length : int
    }

[<Struct>]
type SourceInfo =
    {
        Span : SourceSpan
        Ctx : PContext
    }


[<Struct>]
type PState =
    {
        Buf : ReadOnlyMemory<byte>
        Off : int
        BasePos : int
        Ctx : PContext
    }

module PState =
    let init (buf: uint8[]) : PState =
        { Buf = ReadOnlyMemory<byte> buf; Off = 0; BasePos = 0; Ctx = Root }

    let absolutePos (st: PState) = st.BasePos + st.Off

    let sourceAtCurrent (st: PState) : SourceInfo =
        {
            Span = {
                Offset = absolutePos st
                Length = 0
            }
            Ctx = st.Ctx
        }

    let sourceAtPrevious st n : SourceInfo =
        {
            Span = {
                Offset = max st.BasePos (absolutePos st - n)
                Length = n
            }
            Ctx = st.Ctx
        }

type PError =
    {
        Source : SourceInfo
        Msg : string
    }

module PError =
    let toString (e: PError) : string =
        let ctxStr =
            match PContext.toDisplayPath e.Source.Ctx with
            | "" -> ""
            | path -> $" Context: {path}"

        let span = e.Source.Span

        let location =
            if span.Length = 0 then
                $"at position {span.Offset}"
            else
                $"at position {span.Offset}, length {span.Length}"

        $"Error while parsing{ctxStr} {location}: {e.Msg}"

type Parser<'a> = PState -> Result<'a * PState, PError>

let mapP f p = fun st ->
    match p st with
    | Ok (a, st2) -> Ok (f a, st2)
    | Error e -> Error e

let (|>>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    mapP f p

let inline ok v st =
    Ok (v, st)

let inline err st msg =
    Error {
        Source = PState.sourceAtCurrent st
        Msg = msg
    }

let inline errBefore st n msg =
    Error {
        Source = PState.sourceAtPrevious st n
        Msg = msg
    }

let inline fail msg : Parser<'a> =
    fun st -> err st msg

let inline failBefore n msg : Parser<'a> =
    fun st -> errBefore st n msg

let inline errBufOverflow st =
    err st "unexpected end of buffer"

let getBuffer : Parser<ReadOnlyMemory<uint8>> =
    fun st -> ok st.Buf st

type PBuilder() =
    member _.Bind(p: Parser<'a>, k:'a -> Parser<'b>) : Parser<'b> =
        fun st ->
            match p st with
            | Ok (a, st2) -> k a st2
            | Error e     -> Error e
    member _.Return(x:'a) : Parser<'a> = fun st -> ok x st
    member _.ReturnFrom(p: Parser<'a>) : Parser<'a> = p
    member _.Zero() : Parser<unit> = fun st -> ok () st
    member _.Delay(f: unit -> Parser<'a>) : Parser<'a> = fun st -> f() st
    member _.Combine(p1: Parser<unit>, p2: Parser<'a>) : Parser<'a> = fun st ->
        match p1 st with
        | Ok ((), st2) -> p2 st2
        | Error e      -> Error e

let parser = PBuilder()

type Located<'a> =
    {
        Value : 'a
        Source : SourceInfo
    }

module Located =
    let value x = x.Value
    let source x = x.Source
    let map f x =
        {
            Value = f x.Value
            Source = x.Source
        }

let private withCtx ctx (p: Parser<'a>) : Parser<'a> =
    fun st ->
        let innerState =
            { st with Ctx = Node(ctx, st.Ctx) }

        match p innerState with
        | Ok (value, innerAfter) ->
            Ok (value, { innerAfter with Ctx = st.Ctx })

        | Error e ->
            Error e

let private located (p: Parser<'a>) : Parser<Located<'a>> =
    fun state ->
        let startOffset = PState.absolutePos state

        match p state with
        | Ok (value, stateAfter) ->
            let endOffset = PState.absolutePos stateAfter

            Ok (
                {
                    Value = value
                    Source = {
                        Span = {
                            Offset = startOffset
                            Length = endOffset - startOffset
                        }
                        Ctx = state.Ctx
                    }
                },
                stateAfter
            )

        | Error e ->
            Error e

let field ctx (p: Parser<'a>) : Parser<Located<'a>> =
    withCtx ctx <|
        located p

let getOffset: Parser<int> =
    fun st -> ok st.Off st

let parseByte : Parser<uint8> =
    fun st ->
        if st.Off >= st.Buf.Length then errBufOverflow st
        else ok st.Buf.Span[st.Off] { st with Off = st.Off + 1 }

let skipByte : Parser<unit> =
    fun st ->
        if st.Off >= st.Buf.Length then
            errBufOverflow st
        else
            ok () { st with Off = st.Off + 1 }

let skipBytes n : Parser<unit> =
    fun st ->
        if n < 0 then
            err st $"cannot skip negative byte count {n}"
        elif st.Off + n > st.Buf.Length then
            errBufOverflow st
        else
            ok () { st with Off = st.Off + n }

let peekByte : Parser<uint8> =
    fun st ->
        if st.Off >= st.Buf.Length then errBufOverflow st
        else ok st.Buf.Span[st.Off] st

let takeMem (n:int) : Parser<ReadOnlyMemory<uint8>> =
    fun st ->
        if st.Off + n > st.Buf.Length then errBufOverflow st
        else
            let mem = st.Buf.Slice(st.Off, n)
            ok mem { st with Off = st.Off + n }

let takeAllMem : Parser<ReadOnlyMemory<uint8>> =
    fun st ->
        let mem = st.Buf.Slice(st.Off)
        ok mem { st with Off = st.Buf.Length }

let expectEnd : Parser<unit> =
    fun st ->
        if st.Off = st.Buf.Length then ok () st
        else err st "expected end of buffer"

let remainder : Parser<int> =
    fun st -> ok (st.Buf.Length - st.Off) st

let runOnSubSlice (n:int) (p: Parser<'a>) : Parser<'a> =
    fun st ->
        if st.Off + n > st.Buf.Length then
            errBufOverflow st
        else
            let subSt =
                {
                    Buf = st.Buf.Slice(st.Off, n)
                    Off = 0
                    BasePos = st.BasePos + st.Off
                    Ctx = st.Ctx
                }

            match p subSt with
            | Ok (a, _) ->
                ok a { st with Off = st.Off + n }

            | Error e ->
                Error e

let parseUntilEnd (p: Parser<'a>) : Parser<'a list> =
    fun st ->
        let rec loop st acc =
             if st.Off >= st.Buf.Length then
                 ok (List.rev acc) st
             else
                 let before = st.Off

                 match p st with
                 | Ok (x, stNext) ->
                     if stNext.Off = before then
                         err stNext "parser made no progress in parseUntilEnd"
                     else
                         loop stNext (x::acc)

                 | Error e ->
                     Error e

        loop st []
