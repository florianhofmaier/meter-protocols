namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser

type NoneHeaderCi =
    | Command
    | SelectionOfDevice
    | ApplicationReset

module NoneHeaderCi =

    let tryParse =
        function
        | 0x51uy -> Some Command
        | 0x52uy -> Some SelectionOfDevice
        | 0x50uy -> Some ApplicationReset
        | _ -> None

type ShortHeaderCi =
    | ResponseShortHeader

module ShortHeaderCi =

    let tryParse =
        function
        | 0x7Auy -> Some ResponseShortHeader
        | _ -> None

type LongHeaderCi =
    | ResponseLongHeader
    | AlarmLongHeader

module LongHeaderCi =

    let tryParse =
        function
        | 0x72uy -> Some ResponseLongHeader
        | 0x75uy -> Some AlarmLongHeader
        | _ -> None

type CiFieldTpl =
    | NoneHeader of NoneHeaderCi
    | ShortHeader of ShortHeaderCi
    | LongHeader of LongHeaderCi

module CiFieldTpl =

    let tryParse value : CiFieldTpl option =
        NoneHeaderCi.tryParse value
        |> Option.map NoneHeader
        |> Option.orElseWith (fun () ->
            ShortHeaderCi.tryParse value
            |> Option.map ShortHeader)
        |> Option.orElseWith (fun () ->
            LongHeaderCi.tryParse value
            |> Option.map LongHeader)

    let parse : Parser<ParsedField<CiFieldTpl>> =
        parseField "CI-Field"
        <| parser {
            let! value = parseU8

            match tryParse value with
            | Some ci ->
                return ci

            | None ->
                return! failBefore 1 $"Unknown CI-Field value: 0x{value:X2}"
        }