namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type FixedLengthUserDataRaw =
    {
        CField: ParsedField<CFieldRaw>
        AField: ParsedField<AFieldRaw>
    }

module FixedLengthUserDataRaw =

    let parse : Parser<ParsedField<FixedLengthUserDataRaw>> =
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
        CField: CField
        AField: AField
    }

module FixedLengthUserData =

    let fromRaw (raw: ParsedField<FixedLengthUserDataRaw>) =
        validator {
            let! cField = CField.fromRaw raw.Value.CField
            let! aField = AField.fromRaw raw.Value.AField

            return {
                CField = cField
                AField = aField
            }
        }

type VariableLengthUserDataRaw =
    {
        CField: ParsedField<CFieldRaw>
        AField: ParsedField<AFieldRaw>
        LinkUserData: ParsedField<LinkUserDataRaw>
    }

module VariableLengthUserDataRaw =

    let parse : Parser<ParsedField<VariableLengthUserDataRaw>> =
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
        CField: CField
        AField: AField
        LinkUserData: LinkUserData
    }

module VariableLengthUserData =

    let fromRaw (raw: ParsedField<VariableLengthUserDataRaw>) =
        validator {
            let! cField = CField.fromRaw raw.Value.CField
            let! aField = AField.fromRaw raw.Value.AField
            let! linkUserData = LinkUserData.fromRaw raw.Value.LinkUserData

            return {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }
        }