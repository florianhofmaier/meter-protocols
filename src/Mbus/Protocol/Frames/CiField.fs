namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser

type CiDirection =
    | ToDevice
    | FromDevice
    | Bidirectional

type CiFieldEll =
    | NoDllEncryption

type CiLowerLayerManagement =
    | SelectionOfDevice

type CiFieldTplNoneHeader =
    | ApplicationResetOrSelect
    | Command

type CiFieldTplShortHeader =
    | ApplicationResetOrSelect
    | Response

type CiFieldTplLongHeader =
    | ApplicationResetOrSelect
    | Response
    | Alarm

type CiFieldTpl =
    | NoneHeader of CiFieldTplNoneHeader
    | ShortHeader of CiFieldTplShortHeader
    | LongHeader of CiFieldTplLongHeader

type CiField =
    | Ell of CiFieldEll
    | Nwl
    | Afl
    | LowerLayerManagement of CiLowerLayerManagement
    | Tpl of CiFieldTpl

[<RequireQualifiedAccess>]
type internal CiClassification =
    | Supported of CiField
    | StandardDefinedUnsupportedForWired of byte
    | StandardDefinedNotApplicableToWired of byte
    | Reserved of byte

module private CiClassification =

    let private inRange lower upper value =
        value >= lower && value <= upper

    let private trySupported =
        function
        | 0x50uy ->
            CiFieldTplNoneHeader.ApplicationResetOrSelect
            |> CiFieldTpl.NoneHeader
            |> CiField.Tpl
            |> Some
        | 0x51uy ->
            CiFieldTplNoneHeader.Command
            |> CiFieldTpl.NoneHeader
            |> CiField.Tpl
            |> Some
        | 0x52uy ->
            CiLowerLayerManagement.SelectionOfDevice
            |> CiField.LowerLayerManagement
            |> Some
        | 0x53uy ->
            CiFieldTplLongHeader.ApplicationResetOrSelect
            |> CiFieldTpl.LongHeader
            |> CiField.Tpl
            |> Some
        | 0x57uy ->
            CiFieldTplShortHeader.ApplicationResetOrSelect
            |> CiFieldTpl.ShortHeader
            |> CiField.Tpl
            |> Some
        | 0x72uy ->
            CiFieldTplLongHeader.Response
            |> CiFieldTpl.LongHeader
            |> CiField.Tpl
            |> Some
        | 0x75uy ->
            CiFieldTplLongHeader.Alarm
            |> CiFieldTpl.LongHeader
            |> CiField.Tpl
            |> Some
        | 0x7Auy ->
            CiFieldTplShortHeader.Response
            |> CiFieldTpl.ShortHeader
            |> CiField.Tpl
            |> Some
        | 0x81uy -> Some CiField.Nwl
        | 0x8Cuy -> Some (CiField.Ell CiFieldEll.NoDllEncryption)
        | 0x90uy -> Some CiField.Afl
        | _ -> None

    let private isReserved value =
        inRange 0x20uy 0x4Fuy value
        || inRange 0x58uy 0x59uy value
        || inRange 0x5Duy 0x5Euy value
        || inRange 0x62uy 0x63uy value
        || inRange 0x76uy 0x77uy value
        || inRange 0x91uy 0x9Duy value
        || inRange 0xC6uy 0xFFuy value

    let private isNotApplicableToWired value =
        value = 0x67uy
        || inRange 0x80uy 0x83uy value
        || inRange 0x86uy 0x8Fuy value

    let classify value =
        match trySupported value with
        | Some ci -> CiClassification.Supported ci
        | None when isNotApplicableToWired value ->
            CiClassification.StandardDefinedNotApplicableToWired value
        | None when isReserved value ->
            CiClassification.Reserved value
        | None ->
            CiClassification.StandardDefinedUnsupportedForWired value

    let private supportedValues =
        "Supported TPL CI values are 0x50, 0x51, 0x53, 0x57, 0x72, 0x75, and 0x7A."

    let private isCurrentApplicationCi value =
        match value with
        | 0x54uy | 0x55uy | 0x56uy | 0x66uy | 0x67uy | 0x68uy -> true
        | _ -> false

    let private reference value =
        if isCurrentApplicationCi value then
            "EN 13757-3:2025, Clause 7.3, Table 27."
        else
            "EN 13757-7:2018, 5.2, Table 2."

    let message =
        function
        | CiClassification.StandardDefinedUnsupportedForWired value ->
            $"Unsupported, but standard-conformant wired TPL CI value 0x{value:X2}. "
            + "The value is applicable to wired M-Bus. Field: CI-Field TPL. "
            + supportedValues + " " + reference value
        | CiClassification.StandardDefinedNotApplicableToWired value ->
            $"TPL CI value 0x{value:X2} is standard-defined but not applicable to wired M-Bus. "
            + "Field: CI-Field TPL. " + supportedValues + " " + reference value
        | CiClassification.Reserved value ->
            $"Reserved TPL CI value 0x{value:X2}. Field: CI-Field TPL. "
            + supportedValues + " EN 13757-7:2018, 5.2, Table 2."
        | CiClassification.Supported _ ->
            invalidArg "classification" "A supported CI has no failure message."

module CiField =

    let code =
        function
        | Tpl (NoneHeader CiFieldTplNoneHeader.ApplicationResetOrSelect) -> 0x50uy
        | Tpl (NoneHeader CiFieldTplNoneHeader.Command) -> 0x51uy
        | LowerLayerManagement CiLowerLayerManagement.SelectionOfDevice -> 0x52uy
        | Tpl (LongHeader CiFieldTplLongHeader.ApplicationResetOrSelect) -> 0x53uy
        | Tpl (ShortHeader CiFieldTplShortHeader.ApplicationResetOrSelect) -> 0x57uy
        | Tpl (LongHeader CiFieldTplLongHeader.Response) -> 0x72uy
        | Tpl (LongHeader CiFieldTplLongHeader.Alarm) -> 0x75uy
        | Tpl (ShortHeader CiFieldTplShortHeader.Response) -> 0x7Auy
        | Nwl -> 0x81uy
        | Ell CiFieldEll.NoDllEncryption -> 0x8Cuy
        | Afl -> 0x90uy

    let private fromByte value =
        parser {
            match CiClassification.classify value with
            | CiClassification.Supported ci -> return ci
            | classification ->
                return! failBefore 1 (CiClassification.message classification)
        }

    let private peekFromByte value =
        parser {
            match CiClassification.classify value with
            | CiClassification.Supported ci -> return ci
            | classification ->
                return! fail (CiClassification.message classification)
        }

    let peek : Parser<CiField> =
        parser {
            let! value = peekU8
            return! peekFromByte value
        }

    let parse : Parser<Field<CiField>> =
        parseField "CI-Field"
        <| parser {
            let! value = parseU8
            return! fromByte value
        }

module CiFieldTpl =

    let value ci =
        CiField.code (CiField.Tpl ci)

    /// EN 13757-7:2018, 5.2, Table 2; EN 13757-3:2025, 7.3, Table 27.
    let direction =
        function
        | NoneHeader _
        | ShortHeader CiFieldTplShortHeader.ApplicationResetOrSelect
        | LongHeader CiFieldTplLongHeader.ApplicationResetOrSelect ->
            CiDirection.ToDevice
        | ShortHeader CiFieldTplShortHeader.Response
        | LongHeader CiFieldTplLongHeader.Response
        | LongHeader CiFieldTplLongHeader.Alarm ->
            CiDirection.FromDevice

    let internal isAfl value =
        CiClassification.classify value = CiClassification.Supported CiField.Afl

    let parse : Parser<Field<CiFieldTpl>> =
        parseField "CI-Field TPL"
        <| parser {
            let! raw = parseU8

            match CiClassification.classify raw with
            | CiClassification.Supported (CiField.Tpl ci) -> return ci
            | CiClassification.Supported CiField.Afl ->
                return!
                    failBefore 1
                        "AFL CI value 0x90 is not a TPL CI value. Field: CI-Field TPL. EN 13757-7:2018, 5.2, Table 2."
            | CiClassification.Supported other ->
                return!
                    failBefore 1
                        $"CI value 0x{CiField.code other:X2} does not select the transport layer."
            | classification ->
                return! failBefore 1 (CiClassification.message classification)
        }
