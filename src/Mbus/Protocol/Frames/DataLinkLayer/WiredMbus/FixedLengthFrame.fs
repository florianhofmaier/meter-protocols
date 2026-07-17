namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type FixedLengthFrameRaw =
    {
        UserData: ParsedField<FixedLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: ParsedField<Crc>
        End: ParsedField<EndFieldRaw>
    }

module FixedLengthFrameRaw =

    let parse : Parser<ParsedField<FixedLengthFrameRaw>> =
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
        CField: CField
        AField: AField
    }

module FixedLengthFrame =

    let fromRaw
        (raw: ParsedField<FixedLengthFrameRaw>)
        : Validation<FixedLengthFrame> =

        validator {
            let! userData = FixedLengthUserData.fromRaw raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return! passed {
                CField = userData.CField
                AField = userData.AField
            }
        }