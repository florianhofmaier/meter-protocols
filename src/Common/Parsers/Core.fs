module Metering.Common.Decoding.Parsers.Core

open Metering.Common.Decoding.Parsers.Types

type ParserContext = {
    Reader: IByteReader
    Source: SourceId
    Sources: ISourceStore
    Trace: IFieldTracer
}

type Parser<'a> = ParserContext -> 'a

let private result (value: 'a) : Parser<'a> = fun _ -> value

/// <summary>Transforms a parser's result while using the same parser context.</summary>
let map (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> = fun ctx -> p ctx |> f

/// <summary>Infix form of map; transforms the result of the parser on the left.</summary>
let (|>>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> = map p f

/// <summary>Runs a parser, then runs the parser selected by its result in the same context.</summary>
let bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun ctx ->
        let value = p ctx
        f value ctx

/// <summary>Infix form of bind; selects the next parser from the result of the parser on the left.</summary>
let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> = bind p f

type ParserBuilder() =

    /// <summary>Returns a value without reading input or changing the parser context.</summary>
    member _.Return(value: 'a) : Parser<'a> = result value

    /// <summary>Uses an existing parser as the result of the computation expression.</summary>
    member _.ReturnFrom(parser: Parser<'a>) : Parser<'a> = parser

    /// <summary>Runs a parser and passes its result to the next parser in the same context.</summary>
    member _.Bind(parser: Parser<'a>, next: 'a -> Parser<'b>) : Parser<'b> = bind parser next

    /// <summary>Runs a parser and transforms its result for a final return expression.</summary>
    member _.BindReturn(parser: Parser<'a>, mapFn: 'a -> 'b) : Parser<'b> = map parser mapFn

    /// <summary>Returns unit without reading input or changing the parser context.</summary>
    member _.Zero() : Parser<unit> = result ()

    /// <summary>Runs two parsers in sequence, discarding the first parser's unit result.</summary>
    member _.Combine(first: Parser<unit>, second: Parser<'a>) : Parser<'a> =
        bind first (fun () -> second)

    /// <summary>Defers construction of a parser until that parser is executed.</summary>
    member _.Delay(factory: unit -> Parser<'a>) : Parser<'a> = fun ctx -> factory () ctx

/// <summary>Builds parser computation expressions that share a parser context.</summary>
let parser = ParserBuilder()
