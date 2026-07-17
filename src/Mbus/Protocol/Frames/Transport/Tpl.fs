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

    let private mapCi value (ci: ParsedField<CiFieldTpl>) =
        ci
        |> ParsedField.map (fun _ -> value)

    let parse : Parser<ParsedField<TplRaw>> =
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
            NoneTplHeader tpl.Ci

        | Tpl.ShortHeader tpl ->
            ShortTplHeader tpl.Ci

        | Tpl.LongHeader tpl ->
            LongTplHeader tpl.Ci

    let aplData =
        function
        | Tpl.NoneHeader tpl ->
            tpl.AplData

        | Tpl.ShortHeader tpl ->
            tpl.AplData

        | Tpl.LongHeader tpl ->
            tpl.AplData

    let fromRaw
        (raw: ParsedField<TplRaw>)
        : Validation<Tpl> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tplNone ->
                return! tplNone |> TplWithNoneHeader.fromRaw |> map Tpl.NoneHeader

            | TplRaw.ShortHeader tplShort ->
                return! tplShort |> TplWithShortHeader.fromRaw |> map Tpl.ShortHeader

            | TplRaw.LongHeader tplLong ->
                return! tplLong |> TplWithLongHeader.fromRaw |> map Tpl.LongHeader
        }
