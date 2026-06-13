namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Mbus.Protocol.Frames.Apl
open Metering.Mbus.Protocol.Frames.Tpl

type LinkUserDataRaw =
    {
        Tpl: ParsedField<TplRaw>
        Apl: ParsedField<AplRaw option>
    }

module LinkUserDataRaw =

    let parse : Parser<ParsedField<LinkUserDataRaw>> =
        parseField "User Data"
        <| parser {
            let! tpl = TplRaw.parse
            let! apl = AplRaw.parse

            return
                {
                    Tpl = tpl
                    Apl = apl
                }
        }