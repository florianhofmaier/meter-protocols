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

let private trailingInputError (source: SourceId) (reader: IByteReader) : ParserError =

    {
        Source = source
        Pos = reader.Position
        Msg = $"Exact parsing left trailing input. {reader.Remaining} byte(s) remaining."
    }

/// <summary>Runs a parser with the supplied reader, source store and tracer, assigning the given source to errors whose source is unknown; trailing input is allowed.</summary>
let runWithSource
    (source: SourceId)
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context = createContext source reader sources trace

    execute parser context |> Result.mapError (ParserError.withDefaultSource source)

/// <summary>Runs a parser using the default root source identifier; trailing input is allowed.</summary>
let run
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runWithSource SourceId.root reader sources trace parser

/// <summary>Runs a parser with an explicit source identifier and requires the reader to be fully consumed, returning a parser error for trailing input.</summary>
let runExactlyWithSource
    (source: SourceId)
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context = createContext source reader sources trace

    execute parser context
    |> Result.mapError (ParserError.withDefaultSource source)
    |> Result.bind (fun value ->
        if reader.Remaining = 0 then
            Ok value
        else
            reader |> trailingInputError source |> Error)

/// <summary>Runs a parser using the default root source identifier and requires the reader to be fully consumed.</summary>
let runExactly
    (reader: IByteReader)
    (sources: ISourceStore)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runExactlyWithSource SourceId.root reader sources trace parser
