module Metering.Common.Decoding.Parsers.ParserRunner

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let runWithSource
    (source: SourceId)
    (reader: IByteReader)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        {
            Reader = reader
            Source = source
            Trace = trace
        }

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
