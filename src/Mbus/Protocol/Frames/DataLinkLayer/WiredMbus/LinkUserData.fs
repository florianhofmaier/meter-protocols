namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.Transport

type LinkUserDataRaw =
    {
        Tpl: Field<TplRaw>
    }

module LinkUserDataRaw =

    let parse : Parser<Field<LinkUserDataRaw>> =
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
        Tpl: Field<Tpl>
    }

module LinkUserData =

    let fromRaw
        (raw: Field<LinkUserDataRaw>)
        : Validation<Field<LinkUserData>> =

        validator {
            let! tpl = Tpl.fromRaw raw.Value.Tpl

            return
                raw
                |> Field.withValue {
                    Tpl = tpl
                }
        }
