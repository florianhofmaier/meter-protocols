namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type TplRaw =
    | NoneHeader of TplWithNoneHeaderRaw
    | ShortHeader of TplWithShortHeaderRaw
    | LongHeader of TplWithLongHeaderRaw

module TplRaw =

    let parse : Parser<Field<TplRaw>> =
        parseField "TPL"
        <| parser {
            let! ci = CiFieldTpl.parse

            match ci.Value with
            | CiFieldTpl.NoneHeader noneCi ->
                return!
                    TplWithNoneHeaderRaw.parse (ci |> Field.withValue noneCi)
                    |>> TplRaw.NoneHeader
            | CiFieldTpl.ShortHeader shortCi ->
                return!
                    TplWithShortHeaderRaw.parse (ci |> Field.withValue shortCi)
                    |>> TplRaw.ShortHeader
            | CiFieldTpl.LongHeader longCi ->
                return!
                    TplWithLongHeaderRaw.parse (ci |> Field.withValue longCi)
                    |>> TplRaw.LongHeader
        }

    let ci =
        function
        | NoneHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.NoneHeader
        | ShortHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.ShortHeader
        | LongHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.LongHeader

    let aplData =
        function
        | NoneHeader tpl -> tpl.AplData
        | ShortHeader tpl -> tpl.AplData
        | LongHeader tpl -> tpl.AplData

    let private createMode5Ctx
        (resolver: IExternalSecurityContextResolver)
        (header: LongHeaderMode5Raw) =

        let meterAddress: DeviceIdentificationRaw = {
            IdNum = header.IdNum
            Mfr = header.Mfr
            Version = header.Version
            DevType = header.DevType
        }

        Mode5SecurityContext.create meterAddress header.Acc.Value resolver

    let expand
        (resolver: IExternalSecurityContextResolver)
        (tpl: Field<TplRaw>)
        : Parser<Field<AplDataExpanded>> =

        let originalSource =
            tpl.Value
            |> aplData
            |> AplDataRaw.toParserSource

        tpl
        |> Field.mapParser (
            parser {
                match tpl.Value with
                | NoneHeader _
                | ShortHeader { Header = { Value = ShortHeaderRaw.Mode0Raw _ } }
                | LongHeader { Header = { Value = LongHeaderRaw.Mode0Raw _ } } ->
                    return Unprotected originalSource

                | ShortHeader { Header = { Value = ShortHeaderRaw.Mode5Raw _ } } ->
                    return AplDataExpanded.Protected {
                        Bytes = originalSource
                        Error =
                            failure tpl "Short header with Mode 5 is not supported."
                            |> Validation
                    }

                | LongHeader { Header = { Value = LongHeaderRaw.Mode5Raw header } } ->
                    match createMode5Ctx resolver header with
                    | Error error ->
                        return AplDataExpanded.Protected {
                            Bytes = originalSource
                            Error = error
                        }

                    | Ok ctx ->
                        return! Mode5.expandLongHeader header ctx originalSource
            }
    )

type Tpl =
    | NoneHeader of TplWithNoneHeader
    | ShortHeader of TplWithShortHeader
    | LongHeader of TplWithLongHeader

module Tpl =

    let fromRaw
        (raw: Field<TplRaw>)
        : Validation<Tpl> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tpl ->
                return!
                    TplWithNoneHeader.fromRaw tpl
                    |> map Tpl.NoneHeader

            | TplRaw.ShortHeader tpl ->
                return!
                    TplWithShortHeader.fromRaw tpl
                    |> map Tpl.ShortHeader

            | TplRaw.LongHeader tpl ->
                return!
                    TplWithLongHeader.fromRaw tpl
                    |> map Tpl.LongHeader
        }
