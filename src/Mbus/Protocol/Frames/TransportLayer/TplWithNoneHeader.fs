namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw =
    {
        Ci: Field<CiFieldTplNoneHeader>
        AplData: Field<AplDataRaw>
    }

module TplWithNoneHeaderRaw =

    let parse
        (ci: Field<CiFieldTplNoneHeader>)
        : Parser<TplWithNoneHeaderRaw> =

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
        Ci: Field<CiFieldTplNoneHeader>
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
