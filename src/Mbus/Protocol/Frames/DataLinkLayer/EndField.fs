namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type EndFieldRaw =
    private EndField of uint8

module EndFieldRaw =

    let value (EndField v) =
        v

    let parse : Parser<ParsedField<EndFieldRaw>> =
        parseField "End Field" parseU8
        |>> ParsedField.map EndField

module EndField =

    let value = 0x16uy

    let validate raw =
        ensure
            raw
            $"End field is expected to be 0x16, but it's 0x{raw.Value:X2}"
            (EndFieldRaw.value raw.Value = value)