module Mbus.BaseParsers.Core

open System

type PState = { Buf: ReadOnlyMemory<uint8>; Off: int }
module PState =
    let init (buf: uint8[]) : PState =
        { Buf = ReadOnlyMemory<byte> buf; Off = 0 }

type PError = { Pos: int; Msg: string; Ctx: string list }
module PError =
    let toString (e: PError) : string =
        let ctxStr =
            match e.Ctx with
            | [] -> ""
            | ctx -> " Context: " + (String.concat "." (List.rev ctx))
        $"Error while parsing {ctxStr} at position {e.Pos}: {e.Msg}"

type Parser<'a> = PState -> Result<'a * PState, PError>

let mapP f p = fun st ->
    match p st with
    | Ok (a, st2) -> Ok (f a, st2)
    | Error e -> Error e

let (|>>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    mapP f p

let inline ok v st = Ok (v, st)
let inline err st msg = Error { Pos = st.Off; Msg = msg; Ctx = [] }
let inline fail msg : Parser<'a> = fun st -> err st msg
let inline failBefore msg : Parser<'a> = fun st -> err { st with Off = st.Off - 1 } msg
let inline errBufOverflow st =
    Error { Pos = st.Buf.Length; Msg = "unexpected end of buffer"; Ctx = [] }
let getBuffer : Parser<ReadOnlyMemory<uint8>> = fun st -> ok st.Buf st

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

let withCtx ctx p : Parser<'a> = fun st ->
    match p st with
    | Ok r -> Ok r
    | Error e -> Error { e with Ctx = ctx :: e.Ctx }

let pos : Parser<int> = fun st -> ok st.Off st

let parseByte : Parser<uint8> =
    fun st ->
        if st.Off >= st.Buf.Length then errBufOverflow st
        else ok st.Buf.Span[st.Off] { st with Off = st.Off + 1 }

let skipByte : Parser<unit> =
    fun st -> ok () { st with Off = st.Off + 1 }

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

let remainder : Parser<int> =
    fun st -> ok (st.Buf.Length - st.Off) st

let runOnSubSlice (n:int) (p: Parser<'a>) : Parser<'a> =
    fun st ->
        if st.Off + n > st.Buf.Length then errBufOverflow st
        else
            let subSt = { Buf = st.Buf.Slice(st.Off, n); Off = 0 }
            match p subSt with
            | Ok (a, _) ->
                 ok a { st with Off = st.Off + n }
            | Error e ->
                 Error { e with Pos = st.Off + e.Pos }

let parseUntilEnd (p: Parser<'a>) : Parser<'a list> =
    fun st ->
        let rec loop st acc =
             if st.Off >= st.Buf.Length then
                 ok (List.rev acc) st
             else
                 match p st with
                 | Ok (x, stNext) -> loop stNext (x::acc)
                 | Error e -> Error e
        loop st []
