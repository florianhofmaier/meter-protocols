namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithShortHeaderRaw =
    {
        Ci: Field<CiFieldTplShortHeader>
        Header: Field<ShortHeaderRaw>
        AplData: Field<AplDataRaw>
    }

module TplWithShortHeaderRaw =

    let parse
        (ci: Field<CiFieldTplShortHeader>)
        : Parser<TplWithShortHeaderRaw> =

        parser {
            let! header = ShortHeaderRaw.parse
            let! aplData =
                parseField "APL Data" (takeAll |>> AplDataRaw.create)

            return { Ci = ci; Header = header; AplData = aplData }
        }

type TplWithShortHeader =
    {
        Ci: Field<CiFieldTplShortHeader>
        Header: Field<ShortHeader>
    }

module TplWithShortHeader =

    let fromRaw (raw: TplWithShortHeaderRaw) =
        ShortHeader.fromRaw raw.Header
        |> map (fun header -> { Ci = raw.Ci; Header = header })
