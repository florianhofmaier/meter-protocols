namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.DataLinkLayer

type FixedLengthUserDataRaw = {
    CField: Field<CFieldRaw>
    AField: Field<AFieldRaw>
}

module FixedLengthUserDataRaw =

    let parse: Parser<Field<FixedLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse

            return { CField = cField; AField = aField }
        }

type FixedLengthFrameRaw = {
    UserData: Field<FixedLengthUserDataRaw>
    CrcBytes: CrcBytes
    Crc: Field<Crc>
    End: Field<EndFieldRaw>
}

module FixedLengthFrameRaw =

    let parse: Parser<Field<FixedLengthFrameRaw>> =
        parseField "Format FT 1.2 Frame With Fixed Length"
        <| parser {
            let! _ = StartFixedLength.parse

            let! userDataStart = position

            let! userData = FixedLengthUserDataRaw.parse

            let! crcBytes = bufferSliceAt userDataStart 2 |>> CrcBytes.create

            let! crc = Crc.parse

            let! endField = EndFieldRaw.parse

            return {
                UserData = userData
                CrcBytes = crcBytes
                Crc = crc
                End = endField
            }
        }

type FixedLengthUserData = {
    CField: Field<CField>
    AField: Field<AField>
}

module FixedLengthUserData =

    let validate (raw: Field<FixedLengthUserDataRaw>) : Validation<Field<FixedLengthUserData>> =

        validator {
            let! cField = raw.Value.CField |> validateField CField.validate

            and! aField = raw.Value.AField |> validateField AField.validate

            return raw |> Field.withValue { CField = cField; AField = aField }
        }

    /// Writes the fixed frame's control and address fields.
    let write (data: FixedLengthUserData) =
        writer {
            do! CField.write data.CField.Value
            do! AField.write data.AField.Value
        }

type FixedLengthFrame = {
    CField: Field<CField>
    AField: Field<AField>
}

module FixedLengthFrame =

    let validate (raw: Field<FixedLengthFrameRaw>) : Validation<Field<FixedLengthFrame>> =

        validator {
            let! userData = FixedLengthUserData.validate raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = userData.Value.CField
                    AField = userData.Value.AField
                }
        }

    /// Writes a complete fixed-length frame at the current position, generating checksum and end byte.
    /// IEC 60870-5-2, 3.2, p. 14; EN 13757-2, Annex F, p. 37.
    let write control address : Writer<unit> =
        writer {
            do! writeU8 0x10uy
            let! userDataStart = getPos
            do! control
            do! AField.write address
            let! bytes = getMem userDataStart 2
            do! writeU8 (bytes |> CrcBytes.create |> CrcCalculator.calculate)
            do! writeU8 EndField.value
        }
