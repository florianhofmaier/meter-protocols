namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type VariableLengthUserDataRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
        LinkUserData: LinkUserDataRaw
    }

module VariableLengthUserDataRaw =

    let parse
        (securityContextResolver: IExternalSecurityContextResolver)
        : Parser<Field<VariableLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! linkUserData = LinkUserDataRaw.parse securityContextResolver

            return {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }
        }

type VariableLengthFrameRaw =
    {
        UserData: Field<VariableLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module VariableLengthFrameRaw =

    let parse
        (securityContextResolver: IExternalSecurityContextResolver)
        : Parser<Field<VariableLengthFrameRaw>> =
        parseField "Format FT 1.2 Frame With Variable Length"
        <| parser {
            let! _ = StartVariableLength.parse

            let! lField = LField.parse

            let! _ = StartVariableLength.parse

            let len =
                LField.value lField.Value
                |> int

            let! userDataStart = position

            let! userData =
                runOnSubSlice
                    len
                    (VariableLengthUserDataRaw.parse securityContextResolver)

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

type VariableLengthFrame =
    {
        CField: Field<CField>
        AField: Field<AField>
        LinkUserData: LinkUserData
    }

module VariableLengthFrame =

    let fromRaw
        (raw: Field<VariableLengthUserDataRaw>)
        : Validation<Field<VariableLengthFrame>> =

        validator {
            let! cField =
                raw.Value.CField
                |> validateField CField.fromRaw

            and! aField =
                raw.Value.AField
                |> validateField AField.fromRaw

            and! linkUserData =
                LinkUserData.fromRaw raw.Value.LinkUserData

            let frame = {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }

            return Field.withValue frame raw
        }