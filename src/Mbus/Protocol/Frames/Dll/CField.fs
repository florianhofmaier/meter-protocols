namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type CFieldRaw =
    private CField of uint8

module CFieldRaw =

    let parse : Parser<ParsedField<CFieldRaw>> =
        parseField "CField" parseU8
        |>> ParsedField.map CField