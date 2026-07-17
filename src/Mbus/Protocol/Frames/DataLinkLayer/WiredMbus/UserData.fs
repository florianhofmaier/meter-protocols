namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
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
        LinkUserData: Field<LinkUserDataRaw>
    }

module VariableLengthUserDataRaw =

    let parse : Parser<Field<VariableLengthUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! linkUserData = LinkUserDataRaw.parse

            return {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }
        }

type VariableLengthUserData =
    {
        CField: Field<CField>
        AField: Field<AField>
        LinkUserData: Field<LinkUserData>
    }

module VariableLengthUserData =

    let fromRaw
        (raw: Field<VariableLengthUserDataRaw>)
        : Validation<Field<VariableLengthUserData>> =

        validator {
            let! cField = CField.fromRaw raw.Value.CField
            let! aField = AField.fromRaw raw.Value.AField
            let! linkUserData = LinkUserData.fromRaw raw.Value.LinkUserData

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                    LinkUserData = linkUserData
                }
        }
