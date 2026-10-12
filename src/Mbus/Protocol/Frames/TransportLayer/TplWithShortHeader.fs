namespace Metering.Mbus.Protocol.Frames.TransportLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames

type TplWithShortHeaderRaw =
    {
        Ci: Field<CiFieldTplShortHeader>
        Header: Field<ShortHeaderRaw>
        AplData: Field<ReadOnlyMemory<byte>>
    }

module TplWithShortHeaderRaw =

    let parse
        (ci: Field<CiFieldTplShortHeader>)
        : Parser<TplWithShortHeaderRaw> =

        parser {
            let! header = ShortHeaderRaw.parse
            let! aplData =
                parseField "APL Data" takeAll

            return { Ci = ci; Header = header; AplData = aplData }
        }

type TplWithShortHeader =
    {
        Ci: Field<CiFieldTplShortHeader>
        Header: Field<ShortHeader>
        AplData: Field<ReadOnlyMemory<byte>>
    }

module TplWithShortHeader =

    let fromRaw
        (raw: Field<TplWithShortHeaderRaw>)
        : Validation<Field<TplWithShortHeader>> =

        validateField ShortHeader.fromRaw raw.Value.Header
        |> map (fun header ->
            raw |> Field.withValue {
                Ci = raw.Value.Ci
                Header = header
                AplData = raw.Value.AplData
            })
