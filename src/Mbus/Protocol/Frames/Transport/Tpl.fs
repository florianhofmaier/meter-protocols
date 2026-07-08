namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw =
    {
        Ci: ParsedField<NoneHeaderCiField>
        Apl: ParsedField<AplDataRaw> option
    }

module TplWithNoneHeaderRaw =

    let parse ci : Parser<TplWithNoneHeaderRaw> =
        parser {
            match ci.Value with
            | ApplicationReset ->
                return
                    {
                        Ci = ci
                        Apl = None
                    }

            | Command
            | SelectionOfDevice ->
                let! apl = AplDataRaw.parse
                return
                    {
                        Ci = ci
                        Apl = Some apl
                    }
        }

type TplWithShortHeaderRaw =
    {
        Ci: ParsedField<ShortHeaderCiField>
        Header: ParsedField<ShortHeaderRaw>
        Apl: ParsedField<AplDataRaw>
    }

module TplWithShortHeaderRaw =

    let parse ci : Parser<TplWithShortHeaderRaw> =
        parser {
            let! header = ShortHeaderRaw.parse
            let! apl = AplDataRaw.parse
            return {
                Ci = ci
                Header = header
                Apl = apl
            }
        }

type TplWithLongHeaderRaw =
    {
        Ci: ParsedField<LongHeaderCiField>
        Header: ParsedField<LongHeaderRaw>
        Apl: ParsedField<AplDataRaw>
    }

module TplWithLongHeaderRaw =

    let parse ci : Parser<TplWithLongHeaderRaw> =
        parser {
            let! header = LongHeaderRaw.parse
            let! apl = AplDataRaw.parse
            return {
                Ci = ci
                Header = header
                Apl = apl
            }
        }

type TplRaw =
    | NoneHeader of TplWithNoneHeaderRaw
    | ShortHeader of TplWithShortHeaderRaw
    | LongHeader of TplWithLongHeaderRaw

type TplShort =
    {
        Ci: ShortHeaderCiField
        Header: ShortHeader
        Apl: ParsedField<AplDataRaw>
    }

type TplLong =
    {
        Ci: LongHeaderCiField
        Header: LongHeader
        Apl: ParsedField<AplDataRaw>
    }

type Tpl =
    | NoneHeader of TplNone
    | ShortHeader of TplShort
    | LongHeader of TplLong

module TplRaw =

    let private mapCi value (ci: ParsedField<CiFieldTpl>) =
        ci |> ParsedField.map (fun _ -> value)

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

module TplNone =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplNone> =

        passed
            {
                Ci = raw.Ci.Value
                Apl = raw.Apl
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
                    Apl = raw.Apl
                }
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
                    Apl = raw.Apl
                }
        }

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

