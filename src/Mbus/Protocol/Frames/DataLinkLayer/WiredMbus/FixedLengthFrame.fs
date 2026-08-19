namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.DataLinkLayer

type FixedLengthUserDataRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
    }

module FixedLengthUserDataRaw =

    let parse : Parser<Field<FixedLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse

            return {
                CField = cField
                AField = aField
            }
        }

type FixedLengthFrameRaw =
    {
        UserData: Field<FixedLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module FixedLengthFrameRaw =

    let parse : Parser<Field<FixedLengthFrameRaw>> =
        parseField "Format FT 1.2 Frame With Fixed Length"
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

type FixedLengthUserData =
    {
        CField: Field<CField>
        AField: Field<AField>
    }

module FixedLengthUserData =

    let fromRaw
        (raw: Field<FixedLengthUserDataRaw>)
        : Validation<Field<FixedLengthUserData>> =

        validator {
            let! cField =
                raw.Value.CField
                |> validateField CField.fromRaw

            let! aField =
                raw.Value.AField
                |> validateField AField.fromRaw

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                }
        }

type FixedLengthFrame =
    {
        CField: Field<CField>
        AField: Field<AField>
    }

module FixedLengthFrame =

    let fromRaw
        (raw: Field<FixedLengthFrameRaw>)
        : Validation<Field<FixedLengthFrame>> =

        validator {
            let! userData = FixedLengthUserData.fromRaw raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = userData.Value.CField
                    AField = userData.Value.AField
                }
        }
