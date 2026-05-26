module Metering.Common.Parsers.Core

open System
open Metering.Common.Parsers
open Metering.Common.Parsers.ErrorHandling
open Metering.Common.Parsers.ParserTree

type Parser<'a> =
    ParserState -> Result<'a * ParserState, ParserError>

let mapP f p = fun st ->
    match p st with
    | Ok (a, st2) -> Ok (f a, st2)
    | Error e -> Error e

let (|>>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    mapP f p

let inline ok v st =
    Ok (v, st)

let inline fail msg : Parser<'a> =
    fun st -> err st msg

let inline failBefore n msg : Parser<'a> =
    fun st -> errBefore st n msg

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

let parseNode
    (name: string)
    (p: Parser<'a>)
    : Parser<Parsed<'a>> =

    fun st ->
        let startOffset =
            ParserState.absolutePos st

        let initialSpan =
            {
                Offset = startOffset
                Length = 0
            }

        let child =
            ChildNode.create
                name
                initialSpan
                st.Current

        ParsedNode.addChildNode child st.Current

        let node =
            Child child

        let innerState =
            { st with Current = node }

        match p innerState with
        | Ok (value, stAfter) ->
            let endOffset =
                ParserState.absolutePos stAfter

            ParsedNode.setSpan
                {
                    Offset = startOffset
                    Length = endOffset - startOffset
                }
                node

            Ok
                (
                    {
                        Value = value
                        Node = node
                    },
                    { stAfter with Current = st.Current }
                )

        | Error error ->
            Error error
