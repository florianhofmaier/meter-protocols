namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithShortHeaderRaw =
    {
        Ci: ParsedField<ShortHeaderCiField>
        Header: ParsedField<ShortHeaderRaw>
        AplData: ParsedField<AplDataRaw>
    }

module TplWithShortHeaderRaw =

    let parse ci : Parser<TplWithShortHeaderRaw> =
        parser {
            let! header = ShortHeaderRaw.parse
            let! aplData = AplDataRaw.parse

            return {
                Ci = ci
                Header = header
                AplData = aplData
            }
        }

type TplWithShortHeader =
    {
        Ci: ShortHeaderCiField
        Header: ShortHeader
        AplData: ParsedField<AplDataRaw>
    }

module TplWithShortHeader =

    let fromRaw
        (raw: TplWithShortHeaderRaw)
        : Validation<TplWithShortHeader> =

        validator {
            let! header = ShortHeader.fromRaw raw.Header

            return
                {
                    Ci = raw.Ci.Value
                    Header = header
                    AplData = raw.AplData
                }
        }