namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type AccessNumberRaw =
    private AccessNumber of uint8

module AccessNumberRaw =

    let value (AccessNumber v) =
        v

    let parse : Parser<ParsedField<AccessNumberRaw>> =
        parseField "AccessNumber" parseU8
        |>> ParsedField.map AccessNumber