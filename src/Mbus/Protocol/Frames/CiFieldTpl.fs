namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser

type NoneHeaderCiField =
    | Command
    | SelectionOfDevice
    | ApplicationResetOrSelectNoHeader

type ShortHeaderCiField =
    | ResponseShortHeader
    | ApplicationResetOrSelectShortHeader

type LongHeaderCiField =
    | ResponseLongHeader
    | AlarmLongHeader
    | ApplicationResetOrSelectLongHeader

type CiFieldTpl =
    | NoneTplHeader of NoneHeaderCiField
    | ShortTplHeader of ShortHeaderCiField
    | LongTplHeader of LongHeaderCiField

type CiDirection =
    | CommandToDevice
    | ResponseFromDevice

[<RequireQualifiedAccess>]
type internal CiClassification =
    | Supported of CiFieldTpl
    | Afl
    | StandardDefinedUnsupported of byte
    | Reserved of byte
    | Unknown of byte

module CiFieldTpl =

    let value =
        function
        | NoneTplHeader ApplicationResetOrSelectNoHeader -> 0x50uy
        | NoneTplHeader Command -> 0x51uy
        | NoneTplHeader SelectionOfDevice -> 0x52uy
        | LongTplHeader ApplicationResetOrSelectLongHeader -> 0x53uy
        | ShortTplHeader ApplicationResetOrSelectShortHeader -> 0x57uy
        | LongTplHeader ResponseLongHeader -> 0x72uy
        | LongTplHeader AlarmLongHeader -> 0x75uy
        | ShortTplHeader ResponseShortHeader -> 0x7Auy

    /// EN 13757-7:2018, 5.2, Table 2; EN 13757-3:2025, 7.3, Table 27.
    let direction =
        function
        | NoneTplHeader Command
        | NoneTplHeader SelectionOfDevice
        | NoneTplHeader ApplicationResetOrSelectNoHeader
        | ShortTplHeader ApplicationResetOrSelectShortHeader
        | LongTplHeader ApplicationResetOrSelectLongHeader ->
            CommandToDevice

        | ShortTplHeader ResponseShortHeader
        | LongTplHeader ResponseLongHeader
        | LongTplHeader AlarmLongHeader ->
            ResponseFromDevice

    let private trySupported =
        function
        | 0x50uy -> Some (NoneTplHeader ApplicationResetOrSelectNoHeader)
        | 0x51uy -> Some (NoneTplHeader Command)
        | 0x52uy -> Some (NoneTplHeader SelectionOfDevice)
        | 0x53uy -> Some (LongTplHeader ApplicationResetOrSelectLongHeader)
        | 0x57uy -> Some (ShortTplHeader ApplicationResetOrSelectShortHeader)
        | 0x72uy -> Some (LongTplHeader ResponseLongHeader)
        | 0x75uy -> Some (LongTplHeader AlarmLongHeader)
        | 0x7Auy -> Some (ShortTplHeader ResponseShortHeader)
        | _ -> None

    let private inRange lower upper value =
        value >= lower && value <= upper

    /// EN 13757-7:2018, 5.2, Table 2. CI 0x57 is overridden by the
    /// newer, more specific EN 13757-3:2025, 7.3, Table 27 assignment.
    let private isReserved value =
        inRange 0x20uy 0x4Fuy value
        || inRange 0x56uy 0x59uy value
        || inRange 0x5Duy 0x5Euy value
        || inRange 0x62uy 0x63uy value
        || inRange 0x76uy 0x77uy value
        || inRange 0x91uy 0x9Duy value
        || inRange 0xC6uy 0xFFuy value

    /// Owns the complete raw CI classification used by the TPL parser.
    /// CI 0x54/0x55 assignments in EN 13757-7:2018, Table 2 refer to
    /// EN 13757-3:2018 and are absent from EN 13757-3:2025, 7.3, Table 27.
    let internal classify value =
        match trySupported value with
        | Some ci ->
            CiClassification.Supported ci

        | None ->
            match value with
            | 0x90uy ->
                CiClassification.Afl

            | 0x54uy
            | 0x55uy ->
                CiClassification.Unknown value

            | value when isReserved value ->
                CiClassification.Reserved value

            | value ->
                CiClassification.StandardDefinedUnsupported value

    let internal isAfl value =
        classify value = CiClassification.Afl

    let private supportedValues =
        "Supported TPL CI values are 0x50, 0x51, 0x52, 0x53, 0x57, 0x72, 0x75, and 0x7A."

    let private classificationMessage =
        function
        | CiClassification.StandardDefinedUnsupported value ->
            $"Unsupported, but standard-conformant TPL CI value 0x{value:X2}. "
            + "Field: CI-Field TPL. "
            + supportedValues
            + " EN 13757-7:2018, 5.2, Table 2."

        | CiClassification.Reserved value ->
            $"Reserved TPL CI value 0x{value:X2}. "
            + "Field: CI-Field TPL. "
            + supportedValues
            + " EN 13757-7:2018, 5.2, Table 2."

        | CiClassification.Unknown value ->
            $"Unknown TPL CI value 0x{value:X2} for EN 13757-3:2025. "
            + "Field: CI-Field TPL. "
            + supportedValues
            + " The EN 13757-7:2018, 5.2, Table 2 assignment refers to EN 13757-3:2018 "
            + "and is not present in EN 13757-3:2025, 7.3, Table 27."

        | CiClassification.Afl ->
            "AFL CI value 0x90 is not a TPL CI value. "
            + "Field: CI-Field TPL. EN 13757-7:2018, 5.2, Table 2."

        | CiClassification.Supported ci ->
            $"TPL CI value 0x{value ci:X2} is supported."

    let parse : Parser<Field<CiFieldTpl>> =
        parseField "CI-Field TPL"
        <| parser {
            let! value = parseU8

            match classify value with
            | CiClassification.Supported ci ->
                return ci

            | classification ->
                return!
                    failBefore
                        1
                        (classificationMessage classification)
        }
