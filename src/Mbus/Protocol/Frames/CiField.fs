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

type CiFieldInfo =
    private {
        Direction: CiDirection
        Applicability: MbusApplicability
    }

type CiFieldEll =
    | NoDllEncryption of CiFieldInfo

type CiLowerLayerManagement =
    | SelectionOfDevice of CiFieldInfo

type CiFieldTplNoneHeader =
    | ApplicationResetOrSelect of CiFieldInfo
    | Command of CiFieldInfo

type CiFieldTplShortHeader =
    | ApplicationResetOrSelect of CiFieldInfo
    | Response of CiFieldInfo

type CiFieldTplLongHeader =
    | ApplicationResetOrSelect of CiFieldInfo
    | Response of CiFieldInfo
    | Alarm of CiFieldInfo

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
            {
                Direction = CiDirection.ToDevice
                Applicability = MbusApplicability.WiredMbusOnly
            }
            |> CiFieldTplNoneHeader.ApplicationResetOrSelect
            |> NoneHeader
            |> Tpl

            0x51uy,
            {
                Direction = CiDirection.ToDevice
                Applicability = MbusApplicability.WiredMbusOnly
            }
            |> CiFieldTplNoneHeader.Command
            |> NoneHeader
            |> Tpl

            0x52uy,
            {
                Direction = CiDirection.ToDevice
                Applicability = MbusApplicability.WiredMbusOnly
            }
            |> CiLowerLayerManagement.SelectionOfDevice
            |> LowerLayerManagement

            0x53uy,
            {
                Direction = CiDirection.ToDevice
                Applicability = MbusApplicability.WiredAndWirelessMbus
            }
            |> CiFieldTplLongHeader.ApplicationResetOrSelect
            |> LongHeader
            |> Tpl

            0x57uy,
            {
                Direction = CiDirection.ToDevice
                Applicability = MbusApplicability.WiredAndWirelessMbus
            }
            |> CiFieldTplShortHeader.ApplicationResetOrSelect
            |> ShortHeader
            |> Tpl

            0x72uy,
            {
                Direction = CiDirection.FromDevice
                Applicability = MbusApplicability.WiredAndWirelessMbus
            }
            |> CiFieldTplLongHeader.Response
            |> LongHeader
            |> Tpl

            0x75uy,
            {
                Direction = CiDirection.FromDevice
                Applicability = MbusApplicability.WiredAndWirelessMbus
            }
            |> CiFieldTplLongHeader.Alarm
            |> LongHeader
            |> Tpl

            0x7Auy,
            {
                Direction = CiDirection.FromDevice
                Applicability = MbusApplicability.WiredAndWirelessMbus
            }
            |> CiFieldTplShortHeader.Response
            |> ShortHeader
            |> Tpl

            0x81uy, Nwl

            0x8Cuy,
            {
                Direction = CiDirection.Bidirectional
                Applicability = MbusApplicability.WirelessMbusOnly
            }
            |> CiFieldEll.NoDllEncryption
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
