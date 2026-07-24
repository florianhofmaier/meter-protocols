namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

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
            let! cField = CField.fromRaw raw.Value.CField
            let! aField = AField.fromRaw raw.Value.AField

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                }
        }

type VariableLengthUserDataRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
        HigherLayerData: Field<ReadOnlyMemory<byte>>
    }

module VariableLengthUserDataRaw =

    let parse : Parser<Field<VariableLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! higherLayerData =
                parseField "Higher Layer Data" takeAll

            return {
                CField = cField
                AField = aField
                HigherLayerData = higherLayerData
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
                    HigherLayerData = raw.Value.HigherLayerData
                }
        }
