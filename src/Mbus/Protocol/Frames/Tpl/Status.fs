namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type StatusRaw =
    private Status of uint8

module StatusRaw =

    let value (Status v) =
        v

    let parse : Parser<ParsedField<StatusRaw>> =
        parseField "Status" parseU8
        |>> ParsedField.map Status