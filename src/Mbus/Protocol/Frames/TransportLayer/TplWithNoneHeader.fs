namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
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
            let! aplData =
                parseField "APL Data" (takeAll |>> AplDataRaw.create)

            return { Ci = ci; AplData = aplData }
        }

type TplWithNoneHeader =
    {
        Ci: Field<CiFieldTplNoneHeader>
    }

module TplWithNoneHeader =

    let fromRaw (raw: TplWithNoneHeaderRaw) =
        passed { Ci = raw.Ci }
