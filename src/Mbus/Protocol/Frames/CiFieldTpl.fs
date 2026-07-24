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

module NoneHeaderCiField =

    let tryMap =
        function
        | 0x51uy -> Some Command
        | 0x52uy -> Some SelectionOfDevice
        | 0x50uy -> Some ApplicationResetOrSelectNoHeader
        | _ -> None

type ShortHeaderCiField =
    | ResponseShortHeader
    | ApplicationResetOrSelectShortHeader

module ShortHeaderCiField =

    let value =
        function
        | ResponseShortHeader -> 0x7Auy
        | ApplicationResetOrSelectShortHeader -> 0x57uy

    let tryMap =
        function
        | 0x7Auy -> Some ResponseShortHeader
        | 0x57uy -> Some ApplicationResetOrSelectShortHeader
        | _ -> None

type LongHeaderCiField =
    | ResponseLongHeader
    | AlarmLongHeader
    | ApplicationResetOrSelectLongHeader

module LongHeaderCiField =

    let value =
        function
        | ResponseLongHeader -> 0x72uy
        | AlarmLongHeader -> 0x75uy
        | ApplicationResetOrSelectLongHeader -> 0x53uy

    let tryMap =
        function
        | 0x72uy -> Some ResponseLongHeader
        | 0x75uy -> Some AlarmLongHeader
        | 0x53uy -> Some ApplicationResetOrSelectLongHeader
        | _ -> None

type CiFieldTpl =
    | NoneTplHeader of NoneHeaderCiField
    | ShortTplHeader of ShortHeaderCiField
    | LongTplHeader of LongHeaderCiField

type CiDirection =
    | CommandToDevice
    | ResponseFromDevice

module CiFieldTpl =

    let value =
        function
        | NoneTplHeader Command -> 0x51uy
        | NoneTplHeader SelectionOfDevice -> 0x52uy
        | NoneTplHeader ApplicationResetOrSelectNoHeader -> 0x50uy
        | ShortTplHeader ci -> ShortHeaderCiField.value ci
        | LongTplHeader ci -> LongHeaderCiField.value ci

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

    let parse : Parser<Field<CiFieldTpl>> =
        parseField "CI-Field TPL"
        <| parser {
            let! value = parseU8

            match NoneHeaderCiField.tryMap value with
            | Some value ->
                return NoneTplHeader value

            | None ->
                match ShortHeaderCiField.tryMap value with
                | Some value ->
                    return ShortTplHeader value

                | None ->
                    match LongHeaderCiField.tryMap value with
                    | Some value ->
                        return LongTplHeader value

                    | None ->
                        return!
                            failBefore
                                1
                                $"Unknown or unsupported TPL CI-field value 0x{value:X2}. EN 13757-7:2018, 5.2, Table 2."
        }
