namespace Metering.Mbus.Protocol.Frames.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type WiredMbusFrameRaw =
    | SingleCharacter of Field<unit>
    | FixedLength of Field<FixedLengthFrameRaw>
    | VariableLength of Field<VariableLengthFrameRaw>

module WiredMbusFrameRaw =

    let parse
        (securityContext: IExternalSecurityContextResolver)
        : Parser<Field<WiredMbusFrameRaw>> =

        parseField "Frame"
        <| parser {
            let! startByte = peekU8

            match startByte with
            | 0x10uy ->
                return!
                    FixedLengthFrameRaw.parse
                    |>> FixedLength
            | 0x68uy ->
                return!
                    VariableLengthFrameRaw.parse securityContext
                    |>> VariableLength
            | 0xE5uy ->
                return!
                    SingleCharacterFrame.parse
                    |>> SingleCharacter
            | value ->
                return! ErrorHandling.fail $"Invalid start byte: 0x{value:X2}"
        }

type WiredMbusFrame =
    | SingleCharacter of Field<unit>
    | FixedLength of Field<FixedLengthFrame>
    | VariableLength of Field<VariableLengthFrame>

module WiredMbusFrame =

    let fromRaw
        (raw: Field<WiredMbusFrameRaw>)
        : Validation<WiredMbusFrame> =

        match raw.Value with
        | WiredMbusFrameRaw.SingleCharacter field ->
            passed (WiredMbusFrame.SingleCharacter field)
        | WiredMbusFrameRaw.FixedLength field ->
            FixedLengthFrame.fromRaw field
            |> map WiredMbusFrame.FixedLength
        | WiredMbusFrameRaw.VariableLength field ->
            V.fromRaw field
            |> map WiredMbusFrame.VariableLength

    let decode
        (securityContext: IExternalSecurityContextResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<WiredMbusFrame> =

        decoder {
            let! raw =
                parse (WiredMbusFrameRaw.parse securityContext) source

            return! validate fromRaw raw
        }
