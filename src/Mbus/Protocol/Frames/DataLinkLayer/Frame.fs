namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

module Confirmation =

    let parse : Parser<ParsedField<unit>> =
        parseField "Confirmation"
        <| expectU8 0xE5uy

type FixedLengthRaw =
    {
        UserData: ParsedField<FixedLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: ParsedField<Crc>
        End: ParsedField<EndFieldRaw>
    }

module FixedLengthRaw =

    let parse : Parser<ParsedField<FixedLengthRaw>> =
        parseField "Fixed Length"
        <| parser {
            let! _ = StartFixedLength.parse

            let! userDataStart = position

            let! userData = FixedLengthUserDataRaw.parse

            let! crcBytes =
                bufferSliceAt userDataStart 2
                |>> CrcBytes.create

            let! crc = Crc.parse

            let! endField = EndFieldRaw.parse

            return {
                UserData = userData
                CrcBytes = crcBytes
                Crc = crc
                End = endField
            }
        }

type FrameFixedLength =
    {
        CField: CField
        AField: AField
    }

module FrameFixedLength =

    let fromRaw
        (raw: ParsedField<FixedLengthRaw>)
        : Validation<FrameFixedLength> =

        validator {
            let! userData = FixedLengthUserData.fromRaw raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return! passed {
                CField = userData.CField
                AField = userData.AField
            }
        }

type VariableLengthRaw =
    {
        UserData: ParsedField<VariableLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: ParsedField<Crc>
        End: ParsedField<EndFieldRaw>
    }

module VariableLengthRaw =

    let parse : Parser<ParsedField<VariableLengthRaw>> =
        parseField "Variable Length"
        <| parser {
            let! _ = StartVariableLength.parse

            let! lField = LField.parse

            let! _ = StartVariableLength.parse

            let len =
                LField.value lField.Value
                |> int

            let! userDataStart = position

            let! userData = runOnSubSlice len VariableLengthUserDataRaw.parse

            let! crcBytes =
                bufferSliceAt userDataStart len
                |>> CrcBytes.create

            let! crc = Crc.parse

            let! endField = EndFieldRaw.parse

            return {
                UserData = userData
                CrcBytes = crcBytes
                Crc = crc
                End = endField
            }
        }

type VariableLength =
    {
        CField: CField
        AField: AField
        LinkUserData: LinkUserData
    }

module VariableLength =

    let fromRaw
        (raw: ParsedField<VariableLengthRaw>)
        : Validation<VariableLength> =

        validator {
            let! userData = VariableLengthUserData.fromRaw raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return! passed {
                CField = userData.CField
                AField = userData.AField
                LinkUserData = userData.LinkUserData
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
                return! fail $"Invalid start byte: 0x%02X{b}"
        }

type Frame =
    | Confirmation
    | FixedLength of FrameFixedLength
    | VariableLength of VariableLength

module Frame =

    let fromRaw
        (raw: ParsedField<FrameRaw>)
        : Validation<Frame> =

        validator {
            match raw.Value with
            | FrameRaw.Confirmation _ ->
                return
                    Frame.Confirmation

            | FrameRaw.FixedLength fixedLength ->
                return!
                    FrameFixedLength.fromRaw fixedLength
                    |> map Frame.FixedLength

            | FrameRaw.VariableLength variableLength ->
                return!
                    VariableLength.fromRaw variableLength
                    |> map Frame.VariableLength
        }