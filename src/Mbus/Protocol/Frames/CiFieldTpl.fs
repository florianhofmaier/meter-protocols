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

module ShortHeaderCiField =

    let value =
        function
        | ResponseShortHeader -> 0x7Auy

    let tryMap =
        function
        | 0x7Auy -> Some ResponseShortHeader
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

module CiFieldTpl =

    let value =
        function
        | NoneTplHeader Command -> 0x51uy
        | NoneTplHeader SelectionOfDevice -> 0x52uy
        | NoneTplHeader ApplicationResetOrSelectNoHeader -> 0x50uy
        | ShortTplHeader ci -> ShortHeaderCiField.value ci
        | LongTplHeader ci -> LongHeaderCiField.value ci

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
                        return! failBefore 1 $"Unknown CI-Field value for TPL: 0x{value:X2}"
        }
