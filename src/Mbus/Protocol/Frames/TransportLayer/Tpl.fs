namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplRaw =
    | NoneHeader of TplWithNoneHeaderRaw
    | ShortHeader of TplWithShortHeaderRaw
    | LongHeader of TplWithLongHeaderRaw

module TplRaw =

    let parse : Parser<Field<TplRaw>> =
        parseField "TPL"
        <| parser {
            let! ci = CiField.parse

            match ci.Value with
            | Tpl (CiFieldTpl.NoneHeader noneCi) ->
                let noneHeaderCi = Field.withValue noneCi ci
                return!
                    TplWithNoneHeaderRaw.parse noneHeaderCi
                    |>> TplRaw.NoneHeader

            | Tpl (CiFieldTpl.ShortHeader shortCi) ->
                let shortHeaderCi = Field.withValue shortCi ci
                return!
                    TplWithShortHeaderRaw.parse shortHeaderCi
                    |>> TplRaw.ShortHeader

            | Tpl (CiFieldTpl.LongHeader longCi) ->
                let longHeaderCi = Field.withValue longCi ci
                return!
                    TplWithLongHeaderRaw.parse longHeaderCi
                    |>> TplRaw.LongHeader

            | _ ->
                let code = CiField.code ci.Value
                return!
                    failBefore
                        1
                        $"Expect CI-Field for TPL, got 0x{code:X2} instead"
        }

type Tpl =
    | NoneHeader of TplWithNoneHeader
    | ShortHeader of TplWithShortHeader
    | LongHeader of TplWithLongHeader

module Tpl =

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
