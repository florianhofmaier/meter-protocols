namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type EndFieldRaw =
    private EndField of uint8

module EndFieldRaw =

    let value (EndField v) =
        v

    let parse : Parser<ParsedField<EndFieldRaw>> =
        parseField "End Field" parseU8
        |>> ParsedField.map EndField