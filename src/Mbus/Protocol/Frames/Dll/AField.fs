namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type AFieldRaw =
    private PrimaryAddress of uint8

module AFieldRaw =

    let parse : Parser<ParsedField<AFieldRaw>> =
        parseField "PrimaryAddress" parseU8
        |>> ParsedField.map PrimaryAddress