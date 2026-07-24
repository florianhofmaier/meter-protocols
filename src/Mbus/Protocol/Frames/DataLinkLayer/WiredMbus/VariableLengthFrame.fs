namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type DllVariableLengthRaw =
    {
        UserData: Field<VariableLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module DllVariableLengthRaw =

    let parse : Parser<Field<DllVariableLengthRaw>> =
        parseField "Variable Length"
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

type DllVariableLength =
    {
        CField: Field<CField>
        AField: Field<AField>
        HigherLayerData: Field<ReadOnlyMemory<byte>>
    }

module DllVariableLength =

    let fromRaw
        (raw: Field<DllVariableLengthRaw>)
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

type VariableLengthFrameRaw = DllVariableLengthRaw
type VariableLengthFrame = DllVariableLength

module VariableLengthFrameRaw =
    let parse = DllVariableLengthRaw.parse

module VariableLengthFrame =
    let fromRaw = DllVariableLength.fromRaw
