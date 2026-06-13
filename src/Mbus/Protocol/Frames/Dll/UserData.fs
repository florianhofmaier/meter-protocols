namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type UserDataRaw =
    {
        CField: ParsedField<CFieldRaw>
        AField: ParsedField<AFieldRaw>
        LinkUserData: ParsedField<LinkUserDataRaw>
    }

module UserDataRaw =

    let parse : Parser<ParsedField<UserDataRaw>> =
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