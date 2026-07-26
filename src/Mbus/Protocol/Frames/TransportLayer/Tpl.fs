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
                return!
                    noneCi
                    |> Field.withValue ci
                    |> TplWithNoneHeaderRaw.parse
                    |>> TplRaw.NoneHeader

            | Tpl (CiFieldTpl.ShortHeader shortCi) ->
                return!
                    shortCi
                    |> Field.withValue ci
                    |> TplWithShortHeaderRaw.parse
                    |>> TplRaw.ShortHeader

            | Tpl (CiFieldTpl.LongHeader longCi) ->
                return!
                    longCi
                    |> Field.withValue ci
                    |> TplWithLongHeaderRaw.parse
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

    // let rawAplData =
    //     function
    //     | TplRaw.NoneHeader tpl ->
    //         tpl.AplData
    //
    //     | TplRaw.ShortHeader tpl ->
    //         tpl.AplData
    //
    //     | TplRaw.LongHeader tpl ->
    //         tpl.AplData

    let fromRaw
        (raw: Field<TplRaw>)
        : Validation<Field<Tpl>> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tplNone ->
                return!
                    tplNone
                    |> TplWithNoneHeader.fromRaw
                    |> map Tpl.NoneHeader
                    |> map (Field.withValue raw)

            | TplRaw.ShortHeader tplShort ->
                return!
                    tplShort
                    |> TplWithShortHeader.fromRaw
                    |> map Tpl.ShortHeader
                    |> map (Field.withValue raw)

            | TplRaw.LongHeader tplLong ->
                return!
                    tplLong
                    |> TplWithLongHeader.fromRaw
                    |> map Tpl.LongHeader
                    |> map (Field.withValue raw)
        }
