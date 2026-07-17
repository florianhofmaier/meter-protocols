module Metering.Common.Decoding.Parsers.ParserRunner

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let private createContext
    (source: SourceId)
    (reader: IByteReader)
    (trace: IFieldTracer)
    : ParserContext =

    {
        Reader = reader
        Source = source
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
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        createContext source reader trace

    try
        Ok (parser context)
    with
    | :? ParserException as ex ->
        ex.Error
        |> ParserError.withDefaultSource source
        |> Error

let run
    (reader: IByteReader)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runWithSource SourceId.root reader trace parser

let runExactlyWithSource
    (source: SourceId)
    (reader: IByteReader)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        createContext source reader trace

    try
        let value =
            parser context

        if reader.Remaining = 0 then
            Ok value
        else
            reader
            |> trailingInputError source
            |> Error
    with
    | :? ParserException as ex ->
        ex.Error
        |> ParserError.withDefaultSource source
        |> Error

let runExactly
    (reader: IByteReader)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    runExactlyWithSource SourceId.root reader trace parser
