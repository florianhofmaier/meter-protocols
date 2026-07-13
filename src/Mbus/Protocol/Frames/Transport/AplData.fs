namespace Metering.Mbus.Protocol.Frames.Transport

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility

type AplDataRaw =
    private | RawAplData of ReadOnlyMemory<uint8>

module AplDataRaw =

    let bytes (RawAplData bytes) =
        bytes

    let toByteField (field: ParsedField<AplDataRaw>) =
        field
        |> ParsedField.map bytes

    let parse : Parser<ParsedField<AplDataRaw>> =
        parseField
            "APL Data"
            takeAll
        |>> ParsedField.map RawAplData
