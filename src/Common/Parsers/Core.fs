module Metering.Common.Decoding.Parsers.Core

open Metering.Common.Decoding.Parsers.Types

type ParserContext =
    {
        Reader : IByteReader
        Trace : IFieldTracer
    }

type Parser<'a> =
    ParserContext -> 'a

let private result (value: 'a) : Parser<'a> =
    fun _ ->
        value

let map (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    fun ctx ->
        p ctx |> f

let (|>>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    map p f

let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun ctx ->
        let value = p ctx
        f value ctx

let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    bind p f

type ParserBuilder() =

    member _.Return(value: 'a) : Parser<'a> =
        result value

    member _.ReturnFrom(parser: Parser<'a>) : Parser<'a> =
        parser

    member _.Bind
        (
            parser: Parser<'a>,
            next: 'a -> Parser<'b>
        ) : Parser<'b> =
        bind parser next

    member _.BindReturn
        (
            parser: Parser<'a>,
            mapFn: 'a -> 'b
        ) : Parser<'b> =
        map parser mapFn

    member _.Zero() : Parser<unit> =
        result ()

    member _.Combine
        (
            first: Parser<unit>,
            second: Parser<'a>
        ) : Parser<'a> =
        bind first (fun () -> second)

    member _.Delay(factory: unit -> Parser<'a>) : Parser<'a> =
        factory ()

let parser =
    ParserBuilder()