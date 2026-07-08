namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser

type NoneHeaderCiField =
    | Command
    | SelectionOfDevice
    | ApplicationReset

module NoneHeaderCiField =

    let value =
        function
        | Command -> 0x51uy
        | SelectionOfDevice -> 0x52uy
        | ApplicationReset -> 0x50uy

    let tryMap =
        function
        | 0x51uy -> Some Command
        | 0x52uy -> Some SelectionOfDevice
        | 0x50uy -> Some ApplicationReset
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

module LongHeaderCiField =

    let value =
        function
        | ResponseLongHeader -> 0x72uy
        | AlarmLongHeader -> 0x75uy

    let tryMap =
        function
        | 0x72uy -> Some ResponseLongHeader
        | 0x75uy -> Some AlarmLongHeader
        | _ -> None

type CiFieldTpl =
    | NoneTplHeader of NoneHeaderCiField
    | ShortTplHeader of ShortHeaderCiField
    | LongTplHeader of LongHeaderCiField

module CiFieldTpl =

    let parse : Parser<ParsedField<CiFieldTpl>> =
        parseField "CI-Field"
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