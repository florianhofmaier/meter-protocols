namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Validators.Core
open Metering.Common.BaseWriters.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Utility.Writer

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
    let parse: Parser<Field<DllRaw>> =
        parser {
            let! start = peekU8

            match start with
            | 0xE5uy ->
                return! SingleCharacterFrame.parse |>> Field.map (fun () -> DllRaw.SingleCharacter)
            | 0x10uy -> return! FixedLengthFrameRaw.parse |>> Field.map DllRaw.FixedLength
            | 0x68uy -> return! VariableLengthFrameRaw.parse |>> Field.map DllRaw.VariableLength
            | value -> return! fail $"Invalid DLL start byte: 0x{value:X2}"
        }

    let validate (raw: Field<DllRaw>) : Validation<Field<Dll>> =
        match raw.Value with
        | DllRaw.SingleCharacter -> raw |> Field.withValue Dll.SingleCharacter |> passed
        | DllRaw.FixedLength frame ->
            raw
            |> Field.withValue frame
            |> FixedLengthFrame.validate
            |> map (Field.map Dll.FixedLength)
        | DllRaw.VariableLength frame ->
            raw
            |> Field.withValue frame
            |> VariableLengthFrame.validate
            |> map (Field.map Dll.VariableLength)

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Dll>> =
        decoder {
            let! raw = Decoder.parse parse source
            return! Decoder.validate validate raw
        }

    /// <summary>Dispatches to the frame-specific writer and returns the encoded FT 1.2 DLL bytes.</summary>
    /// <remarks>IEC 60870-5-2, 3.2, p. 14; EN 13757-2, Annex F, p. 37. Link-procedure state and higher-layer validity are outside this operation.</remarks>
    let encode (frame: Dll) : Result<byte[], WError> =
        let write =
            match frame with
            | Dll.SingleCharacter -> SingleCharacterFrame.write
            | Dll.FixedLength frame ->
                FixedLengthFrame.write (CField.write frame.CField.Value) frame.AField.Value
            | Dll.VariableLength frame ->
                VariableLengthFrame.write
                    (CField.write frame.CField.Value)
                    frame.AField.Value
                    (LinkUserDataBytes.write frame.LinkUserData.Value)

        runFt12Writer write
