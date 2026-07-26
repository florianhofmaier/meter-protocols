namespace Metering.Mbus.Protocol.Frames

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type WiredMbusFrameRaw =
    | SingleCharacter of Field<unit>
    | FixedLength of Field<FixedLengthFrameRaw>
    | VariableLength of Field<VariableLengthFrameRaw>

module WiredMbusFrameRaw =

    let parse : Parser<Field<WiredMbusFrameRaw>> =
        parseField "Frame"
        <| parser {
            let! startByte = peekU8

            match startByte with
            | 0x10uy ->
                return! FixedLengthFrameRaw.parse |>> FixedLength

            | 0x68uy ->
                return! VariableLengthFrameRaw.parse |>> VariableLength

            | 0xE5uy ->
                return! SingleCharacterFrame.parse |>> SingleCharacter

            | b ->
                return! fail $"Invalid start byte: 0x%02X{b}"
        }

type WiredMbusFrame =
    | SingleCharacter of Field<unit>
    | FixedLength of Field<FixedLengthFrame>
    | VariableLength of Field<VariableLengthFrame>

module WiredMbusFrame =

    let decode
        (securityContext: SecurityContext)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<WiredMbusFrame> =

        decoder {
            let! raw =
                parse WiredMbusFrameRaw.parse source

            match raw.Value with
            | WiredMbusFrameRaw.SingleCharacter singleCharacter ->
                return
                    WiredMbusFrame.SingleCharacter singleCharacter

            | WiredMbusFrameRaw.FixedLength fixedLength ->
                let! fixedLength =
                    validate FixedLengthFrame.fromRaw fixedLength

                return WiredMbusFrame.FixedLength fixedLength

            | WiredMbusFrameRaw.VariableLength variableLength ->
                let! frameRaw =
                    FrameVariableLengthRaw.parseCompleteMessageFromDll variableLength

                let! expandedRaw =
                    FrameVariableLengthExpandedRaw.expand securityContext frameRaw

                let! variableLength =
                    validate FrameVariableLength.fromExpandedRaw expandedRaw

                return WiredMbusFrame.VariableLength variableLength
        }
