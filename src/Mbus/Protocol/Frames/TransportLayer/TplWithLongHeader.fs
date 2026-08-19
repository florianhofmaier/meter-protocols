namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames

type TplWithLongHeaderRaw =
    {
        Ci: Field<CiFieldTplLongHeader>
        Header: Field<LongHeaderRaw>
        AplData: Field<AplDataRaw>
    }

module TplWithLongHeaderRaw =

    let parse
        (ci: Field<CiFieldTplLongHeader>)
        : Parser<TplWithLongHeaderRaw> =

        parser {
            let! header = LongHeaderRaw.parse
            let! aplData =
                parseField "APL Data" (takeAll |>> AplDataRaw.create)

            return { Ci = ci; Header = header; AplData = aplData }
        }

type TplWithLongHeader =
    {
        Ci: Field<CiFieldTplLongHeader>
        Header: Field<LongHeader>
    }

module TplWithLongHeader =

    let fromRaw
        (raw: TplWithLongHeaderRaw)
        : Validation<TplWithLongHeader> =

        validateField LongHeader.fromRaw raw.Header
        |> map (fun header -> { Ci = raw.Ci; Header = header })
