namespace Metering.Mbus.Protocol.Frames.Apl

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Tpl
open Metering.Mbus.Protocol.Records

type RecordsRaw =
    private | Records of RecordRaw list

module RecordsRaw =

    let parse: Parser<RecordsRaw> =
        parseUntilEnd RecordRaw.parse
        |>> Records

type Alarms =
    private | Alarms of uint8

module Alarms =

    let parse : Parser<ParsedField<Alarms>> =
        parseField "Alarms" parseU8
        |>> ParsedField.map Alarms

type ExtendedSelectionOfDeviceRaw =
    private | ExtendedSelectionOfDevice of ReadOnlyMemory<uint8>

module ExtendedSelectionOfDeviceRaw =

    let private parseExt : Parser<ParsedField<ExtendedSelectionOfDeviceRaw>> =
        parseField "Extended Selection"
        <| parser {
            do! expectU8 0x0Cuy
            do! expectU8 0x78uy
            return! take 4
        }
        |>> ParsedField.map ExtendedSelectionOfDevice

    let parse : Parser<ParsedField<ExtendedSelectionOfDeviceRaw> option> =
        parser {
            let! remaining = remaining
            if remaining > 0 then
                return! parseExt |>> Some
            else
                return None
        }

type SelectionOfDeviceRaw =
    {
        IdNum: ParsedField<IdNumberRaw>
        Mfr: ParsedField<ManufacturerRaw>
        Version: ParsedField<VersionRaw>
        DevType: ParsedField<DeviceTypeRaw>
        Extended: ParsedField<ExtendedSelectionOfDeviceRaw> option
    }

module SelectionOfDeviceRaw =

    let parse : Parser<ParsedField<SelectionOfDeviceRaw>> =
        parseField "Selection of Device"
        <| parser {
            let! idNum = IdNumberRaw.parse
            let! mfr = ManufacturerRaw.parse
            let! version = VersionRaw.parse
            let! devType = DeviceTypeRaw.parse
            let! extended = ExtendedSelectionOfDeviceRaw.parse

            return
                {
                    IdNum = idNum
                    Mfr = mfr
                    Version = version
                    DevType = devType
                    Extended = extended
                }
        }

type AplRaw =
    | RspUdData of RecordsRaw
    | AlarmBits of ParsedField<Alarms>
    | SelectedDevice of ParsedField<SelectionOfDeviceRaw>
    | SndUdData of RecordsRaw
    | NoneApl

module AplRaw =

    let parse
        (tpl : TplRaw)
        : Parser<ParsedField<AplRaw>> =

        parseField "APL"
        <| parser {
            match tpl with
            | NoneHeader tpl ->
                match tpl.Ci.Value with
                | Command -> return! RecordsRaw.parse |>> SndUdData
                | SelectionOfDevice -> return! SelectionOfDeviceRaw.parse |>> SelectedDevice
                | ApplicationReset -> return NoneApl
            | ShortHeader tpl ->
                match tpl.Ci.Value with
                | ResponseShortHeader -> return! RecordsRaw.parse |>> RspUdData

            | LongHeader tpl ->
                match tpl.Ci.Value with
                | ResponseLongHeader -> return! RecordsRaw.parse |>> RspUdData
                | AlarmLongHeader -> return! Alarms.parse |>> AlarmBits
        }