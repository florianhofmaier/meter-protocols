namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.DataLinkLayer

type VariableLengthUserDataRaw = {
    CField: Field<CFieldRaw>
    AField: Field<AFieldRaw>
    LinkUserData: Field<ReadOnlyMemory<byte>>
}

module VariableLengthUserDataRaw =

    let parse: Parser<Field<VariableLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! linkUserData = parseField "Link User Data" takeAll

            return {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }
        }

type VariableLengthFrameRaw = {
    Start: Field<unit>
    Length1: Field<uint8>
    Length2: Field<uint8>
    RepeatedStart: Field<unit>
    UserData: Field<VariableLengthUserDataRaw>
    CrcBytes: CrcBytes
    Crc: Field<Crc>
    End: Field<EndFieldRaw>
}

module VariableLengthFrameRaw =

    /// IEC 60870-5-2, 3.2; EN 13757-2, Annex F.2.
    /// Only the DLL is parsed. Link User Data remains opaque.
    let parse: Parser<Field<VariableLengthFrameRaw>> =
        parseField "Format FT 1.2 Frame With Variable Length"
        <| parser {
            let! start = StartVariableLength.parse
            let! length1 = parseField "Length 1" parseU8
            let! length2 = parseField "Length 2" parseU8
            let! repeatedStart = StartVariableLength.parse
            let length = int length1.Value
            let! userDataStart = position
            let! userData = runOnSubSlice length VariableLengthUserDataRaw.parse
            let! crcBytes = bufferSliceAt userDataStart length |>> CrcBytes.create
            let! crc = Crc.parse
            let! endField = EndFieldRaw.parse

            return {
                Start = start
                Length1 = length1
                Length2 = length2
                RepeatedStart = repeatedStart
                UserData = userData
                CrcBytes = crcBytes
                Crc = crc
                End = endField
            }
        }

type VariableLengthFrame = {
    CField: Field<CField>
    AField: Field<AField>
    LinkUserData: Field<LinkUserDataBytes>
}

module VariableLengthFrame =

    let validate (raw: Field<VariableLengthFrameRaw>) : Validation<Field<VariableLengthFrame>> =

        validator {
            let! cField = raw.Value.UserData.Value.CField |> validateField CField.validate

            and! aField = raw.Value.UserData.Value.AField |> validateField AField.validate

            and! linkUserData = LinkUserDataBytes.validate raw.Value.UserData.Value.LinkUserData

            and! () =
                ensure
                    raw.Value.Length2
                    $"Length 2 must equal Length 1. Expected 0x{raw.Value.Length1.Value:X2}, but got 0x{raw.Value.Length2.Value:X2}."
                    (raw.Value.Length2.Value = raw.Value.Length1.Value)

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                    LinkUserData = linkUserData
                }
        }

    /// Writes a complete variable-length frame from control and payload writers.
    /// Link User Data are bounded before framing; field provenance is not serialized.
    /// IEC 60870-5-2, 3.2, p. 14; EN 13757-2, Annex F.2, p. 37.
    let write control address payload : Writer<unit> =
        match Writer.run LinkUserDataBytes.maxLength payload with
        | Error error -> writerError error.Msg
        | Ok bytes ->
            match LinkUserDataBytes.create (ReadOnlyMemory bytes) with
            | Error error -> writerError error
            | Ok bounded ->
                writer {
                    let linkUserData = LinkUserDataBytes.bytes bounded
                    let length = byte (linkUserData.Length + 2)
                    do! writeU8 0x68uy
                    do! writeU8 length
                    do! writeU8 length
                    do! writeU8 0x68uy
                    let! userDataStart = getPos
                    do! control
                    do! AField.write address
                    do! LinkUserDataBytes.write bounded
                    let! bytes = getMem userDataStart (int length)
                    do! writeU8 (bytes |> CrcBytes.create |> CrcCalculator.calculate)
                    do! writeU8 EndField.value
                }
