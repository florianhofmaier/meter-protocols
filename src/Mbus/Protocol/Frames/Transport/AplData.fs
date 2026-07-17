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

    let toByteField (field: Field<AplDataRaw>) =
        field
        |> Field.map bytes

    let parse : Parser<Field<AplDataRaw>> =
        parseField
            "APL Data"
            takeAll
        |>> Field.map RawAplData
