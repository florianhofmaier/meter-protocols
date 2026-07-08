namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility

module Confirmation =

    let parse : Parser<ParsedField<unit>> =
        parseField "Confirmation"
        <| expectU8 0xE5uy

type FixedLengthRaw =
    {
        CField: ParsedField<CFieldRaw>
        AField: ParsedField<AFieldRaw>
        Crc: ParsedField<CrcRaw>
        End: ParsedField<EndFieldRaw>
    }

module FixedLengthRaw =

    let parse : Parser<ParsedField<FixedLengthRaw>> =
        parseField "Fixed Length"
        <| parser {
            let! _ = StartFixedLength.parse
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! crc = CrcRaw.parse
            let! endField = EndFieldRaw.parse

            return {
                CField = cField
                AField = aField
                Crc = crc
                End = endField
            }
        }

type VariableLengthRaw =
    {
        Start1: ParsedField<StartVariableLength>
        LField: ParsedField<LField>
        Start2: ParsedField<StartVariableLength>
        CField: ParsedField<CFieldRaw>
        AField: ParsedField<AFieldRaw>
        UserData: ParsedField<UserDataRaw>
        Crc: ParsedField<CrcRaw>
        End: ParsedField<EndFieldRaw>
    }

module VariableLengthRaw =

    let parse : Parser<ParsedField<VariableLengthRaw>> =
        parseField "Variable Length"
        <| parser {
            let! start1 = StartVariableLength.parse
            let! lField = LField.parse
            let! start2 = StartVariableLength.parse
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let len = LField.value lField.Value |> int
            let! userData = runOnSubSlice len UserDataRaw.parse
            let! crc = CrcRaw.parse
            let! endField = EndFieldRaw.parse

            return {
                Start1 = start1
                LField = lField
                Start2 = start2
                CField = cField
                AField = aField
                UserData = userData
                Crc = crc
                End = endField
            }
        }

type FrameRaw =
    | Confirmation of ParsedField<unit>
    | FixedLength of ParsedField<FixedLengthRaw>
    | VariableLength of ParsedField<VariableLengthRaw>

module FrameRaw =

    let parse : Parser<ParsedField<FrameRaw>> =
        parseField "Frame"
        <| parser {
            let! startByte = peekU8

            match startByte with
            | 0x10uy ->
                return! FixedLengthRaw.parse |>> FixedLength

            | 0x68uy ->
                return! VariableLengthRaw.parse |>> VariableLength

            | 0xE5uy ->
                return! Confirmation.parse |>> Confirmation

            | b ->
                return! failBefore 1 $"Invalid start byte: 0x%02X{b}"
        }
