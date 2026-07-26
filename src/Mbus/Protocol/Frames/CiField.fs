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

type MbusApplicability =
    | WiredMbusOnly
    | WirelessMbusOnly
    | WiredAndWirelessMbus

type CiFieldEll =
    | NoDllEncryption

type CiLowerLayerManagement =
    | SelectionOfDevice

type CiFieldTplNoneHeader =
    | ApplicationResetOrSelect
    | Command

type CiFieldTplShortHeader =
    | Response

type CiFieldTplLongHeader =
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

module CiField =

    let private table =
        Map [
            0x50uy,
            CiFieldTplNoneHeader.ApplicationResetOrSelect
            |> NoneHeader
            |> Tpl

            0x51uy,
            CiFieldTplNoneHeader.Command
            |> NoneHeader
            |> Tpl

            0x52uy,
            CiLowerLayerManagement.SelectionOfDevice
            |> LowerLayerManagement

            0x72uy,
            CiFieldTplLongHeader.Response
            |> LongHeader
            |> Tpl

            0x75uy,
            CiFieldTplLongHeader.Alarm
            |> LongHeader
            |> Tpl

            0x7Auy,
            CiFieldTplShortHeader.Response
            |> ShortHeader
            |> Tpl

            0x81uy, Nwl

            0x8Cuy,
            CiFieldEll.NoDllEncryption
            |> Ell

            0x90uy,
            Afl
        ]

    let private codeByField =
        table
        |> Map.toSeq
        |> Seq.map (fun (code, ciField) -> ciField, code)
        |> Map.ofSeq

    let code ciField =
        Map.tryFind ciField codeByField
        |> Option.defaultValue 0xFFuy

    let private getCi value =
        parser {
            match Map.tryFind value table with
            | Some ciField ->
                return ciField

            | None ->
                return! failBefore 1 $"CI-Field 0x{value:X2} is not supported"
        }

    let peek : Parser<CiField> =
        parser {
            let! value = peekU8
            return! getCi value
        }

    let parse : Parser<Field<CiField>> =
        parseField "CI-Field"
        <| parser {
            let! value = parseU8

            match Map.tryFind value table with
            | Some ciField ->
                return ciField

            | None ->
                return! failBefore 1 $"CI-Field 0x{value:X2} is not supported"
        }
