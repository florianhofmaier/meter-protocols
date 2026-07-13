namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.Transport

type LinkUserDataRaw =
    {
        Tpl: ParsedField<TplRaw>
    }

module LinkUserDataRaw =

    let parse : Parser<ParsedField<LinkUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! tpl = TplRaw.parse

            return
                {
                    Tpl = tpl
                }
        }

type LinkUserData =
    {
        Tpl: Tpl
    }

module LinkUserData =

    let fromRaw (raw: ParsedField<LinkUserDataRaw>) =
        validator {
            let! tpl = Tpl.fromRaw raw.Value.Tpl

            return
                {
                    Tpl = tpl
                }
        }
