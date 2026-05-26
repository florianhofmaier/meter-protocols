module Metering.Common.Decoding.Parsers.ParserRunner

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let run
    (reader: IByteReader)
    (trace: IFieldTracer)
    (parser: Parser<'a>)
    : Result<'a, ParserError> =

    let context =
        {
            Reader = reader
            Trace = trace
        }

    try
        Ok (parser context)
    with
    | :? ParserException as ex ->
        Error ex.Error