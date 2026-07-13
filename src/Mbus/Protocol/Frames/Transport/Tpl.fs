namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.Application

type TplWithNoneHeaderRaw =
    {
        Ci: ParsedField<NoneHeaderCiField>
        AplData: ParsedField<AplRaw>
    }

module TplWithNoneHeaderRaw =

    let parse ci : Parser<TplWithNoneHeaderRaw> =
        parser {
            let! aplData =
                ci.Value
                |> NoneTplHeader
                |> AplRaw.parse

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
        AplData: ParsedField<AplRaw>
    }

module TplWithShortHeaderRaw =

    let parse ci : Parser<TplWithShortHeaderRaw> =
        parser {
            let! header = ShortHeaderRaw.parse
            let! aplData =
                ci.Value
                |> ShortTplHeader
                |> AplRaw.parse

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
        AplData: ParsedField<AplRaw>
    }

module TplWithLongHeaderRaw =

    let parse ci : Parser<TplWithLongHeaderRaw> =
        parser {
            let! header = LongHeaderRaw.parse
            let! aplData =
                ci.Value
                |> LongTplHeader
                |> AplRaw.parse

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
        AplData: Apl
    }

module TplNone =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplNone> =

        validator {
            let! aplData = Apl.fromRaw raw.AplData

            return {
                Ci = raw.Ci.Value
                AplData = aplData
            }
        }

type TplShort =
    {
        Ci: ShortHeaderCiField
        Header: ShortHeader
        AplData: Apl
    }

module TplShort =

    let fromRaw
        (raw: TplWithShortHeaderRaw)
        : Validation<TplShort> =

        validator {
            let! header = ShortHeader.fromRaw raw.Header
            let! aplData = Apl.fromRaw raw.AplData

            return
                {
                    Ci = raw.Ci.Value
                    Header = header
                    AplData = aplData
                }
        }

type TplLong =
    {
        Ci: LongHeaderCiField
        Header: LongHeader
        AplData: Apl
    }

module TplLong =

    let fromRaw
        (raw: TplWithLongHeaderRaw)
        : Validation<TplLong> =

        validator {
            let! header = LongHeader.fromRaw raw.Header
            let! aplData = Apl.fromRaw raw.AplData

            return
                {
                    Ci = raw.Ci.Value
                    Header = header
                    AplData = aplData
                }
        }

type Tpl =
    | NoneHeader of TplNone
    | ShortHeader of TplShort
    | LongHeader of TplLong

module Tpl =

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
