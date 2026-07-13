namespace Metering.Mbus.Protocol.Frames.Application

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type AplRaw =
    | RspUdData of RecordsRaw
    | AlarmBits of ParsedField<Alarms>
    | SelectedDevice of ParsedField<SelectionOfDeviceRaw>
    | SndUdData of RecordsRaw
    | ApplicationResetOrSelect of ParsedField<ApplicationResetOrSelectRaw>

module AplRaw =

    let parse
        (ci : CiFieldTpl)
        : Parser<ParsedField<AplRaw>> =

        parseField "APL"
        <| parser {
            match ci with
            | NoneTplHeader noneCi ->
                match noneCi with
                | ApplicationResetOrSelectNoHeader ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | Command ->
                    return! RecordsRaw.parse |>> SndUdData

                | SelectionOfDevice ->
                    return! SelectionOfDeviceRaw.parse |>> SelectedDevice

            | ShortTplHeader shortCi ->
                match shortCi with
                | ApplicationResetOrSelectShortHeader ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | ResponseShortHeader ->
                    return! RecordsRaw.parse |>> RspUdData

            | LongTplHeader longCi ->
                match longCi with
                | ApplicationResetOrSelectLongHeader ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | ResponseLongHeader ->
                    return! RecordsRaw.parse |>> RspUdData

                | AlarmLongHeader ->
                    return! Alarms.parse |>> AlarmBits
        }

type Apl =
    | RspUdData of RspUdData
    | AlarmBits of Alarms
    | SelectedDevice of SelectionOfDevice
    | SndUdData of SndUdData
    | ApplicationResetOrSelect of ApplicationResetOrSelect

module Apl =

    let fromRaw
        (raw: ParsedField<AplRaw>)
        : Validation<Apl> =

        validator {
            match raw.Value with
            | AplRaw.RspUdData records ->
                let! data = RspUdData.fromRaw records
                return RspUdData data

            | AplRaw.SndUdData records ->
                let! data = SndUdData.fromRaw records
                return SndUdData data

            | AplRaw.AlarmBits alarms ->
                return AlarmBits alarms.Value

            | AplRaw.SelectedDevice selection ->
                let! selection = SelectionOfDevice.fromRaw selection
                return SelectedDevice selection

            | AplRaw.ApplicationResetOrSelect raw ->
                let! resetOrSelect = ApplicationResetOrSelect.fromRaw raw
                return ApplicationResetOrSelect resetOrSelect
        }
