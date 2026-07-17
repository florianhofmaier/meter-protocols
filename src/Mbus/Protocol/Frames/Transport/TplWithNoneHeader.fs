namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw =
    {
        Ci: ParsedField<NoneHeaderCiField>
        AplData: ParsedField<AplDataRaw>
    }

module TplWithNoneHeaderRaw =

    let parse ci : Parser<TplWithNoneHeaderRaw> =
        parser {
            let! aplData = AplDataRaw.parse

            return
                {
                    Ci = ci
                    AplData = aplData
                }
        }

type TplWithNoneHeader =
    {
        Ci: NoneHeaderCiField
        AplData: ParsedField<AplDataRaw>
    }

module TplWithNoneHeader =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplWithNoneHeader> =

        validator {
            return {
                Ci = raw.Ci.Value
                AplData = raw.AplData
            }
        }