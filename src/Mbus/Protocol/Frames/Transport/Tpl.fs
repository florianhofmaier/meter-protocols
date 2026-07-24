namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplRaw =
    | NoneHeader of TplWithNoneHeaderRaw
    | ShortHeader of TplWithShortHeaderRaw
    | LongHeader of TplWithLongHeaderRaw

module TplRaw =

    let private mapCi value (ci: Field<CiFieldTpl>) =
        ci
        |> Field.map (fun _ -> value)

    let parse : Parser<Field<TplRaw>> =
        parseField "TPL"
        <| parser {
            let! ci = CiFieldTpl.parse

            match ci.Value with
            | NoneTplHeader noneCi ->
                let noneHeaderCi = mapCi noneCi ci
                return!
                    TplWithNoneHeaderRaw.parse noneHeaderCi
                    |>> TplRaw.NoneHeader

            | ShortTplHeader shortCi ->
                let shortHeaderCi = mapCi shortCi ci
                return!
                    TplWithShortHeaderRaw.parse shortHeaderCi
                    |>> TplRaw.ShortHeader

            | LongTplHeader longCi ->
                let longHeaderCi = mapCi longCi ci
                return!
                    TplWithLongHeaderRaw.parse longHeaderCi
                    |>> TplRaw.LongHeader
        }

type Tpl =
    | NoneHeader of TplWithNoneHeader
    | ShortHeader of TplWithShortHeader
    | LongHeader of TplWithLongHeader

module Tpl =

    let ci =
        function
        | Tpl.NoneHeader tpl ->
            NoneTplHeader tpl.Ci.Value

        | Tpl.ShortHeader tpl ->
            ShortTplHeader tpl.Ci.Value

        | Tpl.LongHeader tpl ->
            LongTplHeader tpl.Ci.Value

    let rawAplData =
        function
        | TplRaw.NoneHeader tpl ->
            tpl.AplData

        | TplRaw.ShortHeader tpl ->
            tpl.AplData

        | TplRaw.LongHeader tpl ->
            tpl.AplData

    let fromRaw
        (raw: Field<TplRaw>)
        : Validation<Field<Tpl>> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tplNone ->
                let! tpl = tplNone |> TplWithNoneHeader.fromRaw
                return raw |> Field.withValue (Tpl.NoneHeader tpl)

            | TplRaw.ShortHeader tplShort ->
                let! tpl = tplShort |> TplWithShortHeader.fromRaw
                return raw |> Field.withValue (Tpl.ShortHeader tpl)

            | TplRaw.LongHeader tplLong ->
                let! tpl = tplLong |> TplWithLongHeader.fromRaw
                return raw |> Field.withValue (Tpl.LongHeader tpl)
        }
