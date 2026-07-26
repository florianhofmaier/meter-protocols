namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.UserData

type VariableLengthUserDataRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
        LinkUserData: Field<ReadOnlyMemory<byte>>
    }

module VariableLengthUserDataRaw =

    let parse : Parser<Field<VariableLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! linkUserData =
                parseField "Link User Data" takeAll

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

type VariableLengthUserDataExpandedRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
        LinkUserData: Field<CompleteLinkUserDataRaw>
    }

type VariableLengthFrameExpandedRaw =
    {
        UserData: Field<VariableLengthUserDataExpandedRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module VariableLengthFrameRaw =

    let parse : Parser<Field<VariableLengthFrameRaw>> =
        parseField "Format FT 1.2 Frame With Variable Length"
        <| parser {
            let! _ = StartVariableLength.parse

            let! lField = LField.parse

            let! _ = StartVariableLength.parse

            let len =
                LField.value lField.Value
                |> int

            let! userDataStart = position

            let! userData = runOnSubSlice len VariableLengthUserDataRaw.parse

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

    let expand
        (raw: Field<VariableLengthFrameRaw>)
        : Decoder<Field<VariableLengthFrameExpandedRaw>> =

        decoder {
            let! userData =
                raw.Value.UserData.Value.LinkUserData
                |> Field.mapError (fun e -> $"User Data: {e}")
                |> VariableLengthUserDataExpandedRaw.fromRaw

            return
                raw
                |> Field.withValue {
                    UserData = userData
                    CrcBytes = raw.Value.CrcBytes
                    Crc = raw.Value.Crc
                    End = raw.Value.End
                }
        }

type VariableLengthUserData =
    {
        CField: Field<CField>
        AField: Field<AField>
        HigherLayerData: Field<ReadOnlyMemory<byte>>
    }

module VariableLengthUserData =

    let fromRaw
        (raw: Field<VariableLengthUserDataRaw>)
        : Validation<Field<VariableLengthUserData>> =

        validator {
            let! cField = CField.fromRaw raw.Value.CField
            and! aField = AField.fromRaw raw.Value.AField

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                    HigherLayerData = raw.Value.LinkUserData
                }
        }

type DllVariableLength =
    {
        CField: Field<CField>
        AField: Field<AField>
        HigherLayerData: Field<ReadOnlyMemory<byte>>
    }

module DllVariableLength =

    let fromRaw
        (raw: Field<VariableLengthFrameRaw>)
        : Validation<Field<DllVariableLength>> =

        validator {
            let! userData =
                VariableLengthUserData.fromRaw raw.Value.UserData

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = userData.Value.CField
                    AField = userData.Value.AField
                    HigherLayerData = userData.Value.HigherLayerData
                }
        }
