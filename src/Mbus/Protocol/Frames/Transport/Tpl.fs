namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
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

type TplWithShortHeaderRaw =
    {
        Ci: ParsedField<ShortHeaderCiField>
        Header: ParsedField<ShortHeaderRaw>
        AplData: ParsedField<AplDataRaw>
    }

module TplWithShortHeaderRaw =

    let parse ci : Parser<TplWithShortHeaderRaw> =
        parser {
            let! header = ShortHeaderRaw.parse
            let! aplData = AplDataRaw.parse

            return {
                Ci = ci
                Header = header
                AplData = aplData
            }
        }

type TplWithLongHeaderRaw =
    {
        Ci: ParsedField<LongHeaderCiField>
        Header: ParsedField<LongHeaderRaw>
        AplData: ParsedField<AplDataRaw>
    }

module TplWithLongHeaderRaw =

    let parse ci : Parser<TplWithLongHeaderRaw> =
        parser {
            let! header = LongHeaderRaw.parse
            let! aplData = AplDataRaw.parse

            return {
                Ci = ci
                Header = header
                AplData = aplData
            }
        }

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

type TplNone =
    {
        Ci: NoneHeaderCiField
        AplData: ParsedField<AplDataRaw>
    }

module TplNone =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplNone> =

        validator {
            return {
                Ci = raw.Ci.Value
                AplData = raw.AplData
            }
        }

type TplShort =
    {
        Ci: ShortHeaderCiField
        Header: ShortHeader
        AplData: ParsedField<AplDataRaw>
    }

module TplShort =

    let fromRaw
        (raw: TplWithShortHeaderRaw)
        : Validation<TplShort> =

        validator {
            let! header = ShortHeader.fromRaw raw.Header

            return
                {
                    Ci = raw.Ci.Value
                    Header = header
                    AplData = raw.AplData
                }
        }

type TplLong =
    {
        Ci: LongHeaderCiField
        Header: LongHeader
        AplData: ParsedField<AplDataRaw>
    }

module TplLong =

    let fromRaw
        (raw: TplWithLongHeaderRaw)
        : Validation<TplLong> =

        validator {
            let! header = LongHeader.fromRaw raw.Header

            return
                {
                    Ci = raw.Ci.Value
                    Header = header
                    AplData = raw.AplData
                }
        }

type Tpl =
    | NoneHeader of TplNone
    | ShortHeader of TplShort
    | LongHeader of TplLong

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
                return! tplNone |> TplNone.fromRaw |> map Tpl.NoneHeader

            | TplRaw.ShortHeader tplShort ->
                return! tplShort |> TplShort.fromRaw |> map Tpl.ShortHeader

            | TplRaw.LongHeader tplLong ->
                return! tplLong |> TplLong.fromRaw |> map Tpl.LongHeader
        }
