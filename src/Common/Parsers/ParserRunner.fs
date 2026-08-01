module Metering.Common.Decoding.Parsers.ParserRunner

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Types

let private createContext
    (source: SourceId)
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    : ParserContext =

    {
        Reader = reader
        Source = source
        Sources = sources
        Trace = trace
    }

let private trailingInputError
    (source: SourceId)
    (reader: IByteReader)
    : ParserError =

    {
        Source = source
        Pos = reader.Position
        Msg =
            $"Exact parsing left trailing input. {reader.Remaining} byte(s) remaining."
    }

let runWithSource
    (source: SourceId)
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        createContext source reader sources trace

    execute parser context
    |> Result.mapError (ParserError.withDefaultSource source)

let run
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runWithSource SourceId.root reader sources trace parser

let runExactlyWithSource
    (source: SourceId)
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        createContext source reader sources trace

    execute parser context
    |> Result.mapError (ParserError.withDefaultSource source)
    |> Result.bind (fun value ->
        if reader.Remaining = 0 then
            Ok value
        else
            reader
            |> trailingInputError source
            |> Error)

let runExactly
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runExactlyWithSource SourceId.root reader sources trace parser
