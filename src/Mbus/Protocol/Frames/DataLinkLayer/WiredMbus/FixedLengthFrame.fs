namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type FixedLengthFrameRaw =
    {
        UserData: Field<FixedLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module FixedLengthFrameRaw =

    let parse : Parser<Field<FixedLengthFrameRaw>> =
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
