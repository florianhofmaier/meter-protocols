namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Protection
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
                    let failures =
                        fail tpl "Short header with Mode 5 is not supported."

                    return Protected {
                        Bytes = originalSource
                        Failure = InvalidFrameStructure failures
                    }

                | LongHeader { Header = { Value = LongHeaderRaw.Mode5Raw header } } ->
                    match createMode5Ctx resolver header with
                    | Error err ->
                        return Protected {
                            Bytes = originalSource
                            Failure = EncryptionError err
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
        : Validation<Field<Tpl>> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tpl ->
                let! valid = TplWithNoneHeader.fromRaw tpl
                return raw |> Field.withValue (Tpl.NoneHeader valid)
            | TplRaw.ShortHeader tpl ->
                let! valid = TplWithShortHeader.fromRaw tpl
                return raw |> Field.withValue (Tpl.ShortHeader valid)
            | TplRaw.LongHeader tpl ->
                let! valid = TplWithLongHeader.fromRaw tpl
                return raw |> Field.withValue (Tpl.LongHeader valid)
        }
