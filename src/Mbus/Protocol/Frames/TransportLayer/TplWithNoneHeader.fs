namespace Metering.Mbus.Protocol.Frames.TransportLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw =
    {
        Ci: Field<CiFieldTplNoneHeader>
        AplData: Field<ReadOnlyMemory<byte>>
    }

module TplWithNoneHeaderRaw =

    let parse
        (ci: Field<CiFieldTplNoneHeader>)
        : Parser<TplWithNoneHeaderRaw> =

        parser {
            let! aplData =
                parseField "APL Data" takeAll

            return { Ci = ci; AplData = aplData }
        }

type TplWithNoneHeader =
    {
        Ci: Field<CiFieldTplNoneHeader>
        AplData: Field<ReadOnlyMemory<byte>>
    }

module TplWithNoneHeader =

    let fromRaw
        (raw: Field<TplWithNoneHeaderRaw>)
        : Validation<Field<TplWithNoneHeader>> =

        raw
        |> Field.withValue { Ci = raw.Value.Ci; AplData = raw.Value.AplData }
        |> passed
