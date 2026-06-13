namespace Metering.Mbus.Protocol.Frames.Dll

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

    let parse : Parser<ParsedField<StartVariableLength>> =
        parseField "Start Field" (expectU8 0x68uy)
        |>> ParsedField.map StartVariableLength