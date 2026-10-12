namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Validators.Core

[<RequireQualifiedAccess>]
type DllRaw =
    | SingleCharacter
    | FixedLength of FixedLengthFrameRaw
    | VariableLength of VariableLengthFrameRaw

[<RequireQualifiedAccess>]
type Dll =
    | SingleCharacter
    | FixedLength of FixedLengthFrame
    | VariableLength of VariableLengthFrame

module DllCodec =

    /// IEC 60870-5-2, 3.2; EN 13757-2, Annex F.1/F.2.
    /// Only DLL structure is read; Link User Data remains opaque.
    let parse : Parser<Field<DllRaw>> =
        parser {
            let! start = peekU8

            match start with
            | 0xE5uy ->
                return! SingleCharacterFrame.parse |>> Field.map (fun () -> DllRaw.SingleCharacter)
            | 0x10uy ->
                return! FixedLengthFrameRaw.parse |>> Field.map DllRaw.FixedLength
            | 0x68uy ->
                return! VariableLengthFrameRaw.parse |>> Field.map DllRaw.VariableLength
            | value ->
                return! fail $"Invalid DLL start byte: 0x{value:X2}"
        }

    let fromRaw (raw: Field<DllRaw>) : Validation<Field<Dll>> =
        match raw.Value with
        | DllRaw.SingleCharacter ->
            raw |> Field.withValue Dll.SingleCharacter |> passed
        | DllRaw.FixedLength frame ->
            raw |> Field.withValue frame
            |> FixedLengthFrame.fromRaw
            |> map (Field.map Dll.FixedLength)
        | DllRaw.VariableLength frame ->
            raw |> Field.withValue frame
            |> VariableLengthFrame.fromRaw
            |> map (Field.map Dll.VariableLength)

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Dll>> =
        decoder {
            let! raw = Core.parse parse source
            return! validate fromRaw raw
        }
