namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type StartFixedLength =
    private StartFixedLength of unit

module StartFixedLength =

    let parse : Parser<ParsedField<StartFixedLength>> =
        parseField "Start Field" (expectU8 0x10uy)
        |>> ParsedField.map StartFixedLength

type StartVariableLength =
    private StartVariableLength of unit

module StartVariableLength =

    let parse : Parser<ParsedField<Unit>> =
        parseField "Start Field" (expectU8 0x68uy)