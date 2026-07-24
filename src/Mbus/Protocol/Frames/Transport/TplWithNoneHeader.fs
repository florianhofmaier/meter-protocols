namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw =
    {
        Ci: Field<NoneHeaderCiField>
        AplData: Field<AplDataRaw>
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
        Ci: Field<NoneHeaderCiField>
    }

module TplWithNoneHeader =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplWithNoneHeader> =

        validator {
            return {
                Ci = raw.Ci
            }
        }
