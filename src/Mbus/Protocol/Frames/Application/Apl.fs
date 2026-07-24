namespace Metering.Mbus.Protocol.Frames.Application

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type AplRaw =
    | RspUdData of RecordsRaw
    | AlarmBits of Field<Alarms>
    | SelectedDevice of Field<SelectionOfDeviceRaw>
    | SndUdData of RecordsRaw
    | ApplicationResetOrSelect of Field<ApplicationResetOrSelectRaw>

module AplRaw =

    let parse
        (ci : CiFieldTpl)
        : Parser<Field<AplRaw>> =

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
        (raw: Field<AplRaw>)
        : Validation<Field<Apl>> =

        validator {
            match raw.Value with
            | AplRaw.RspUdData records ->
                let! data = RspUdData.fromRaw records
                return raw |> Field.withValue (RspUdData data)

            | AplRaw.SndUdData records ->
                let! data = SndUdData.fromRaw records
                return raw |> Field.withValue (SndUdData data)

            | AplRaw.AlarmBits alarms ->
                return raw |> Field.withValue (AlarmBits alarms.Value)

            | AplRaw.SelectedDevice selection ->
                let! selection = SelectionOfDevice.fromRaw selection
                return raw |> Field.withValue (SelectedDevice selection)

            | AplRaw.ApplicationResetOrSelect raw ->
                let! resetOrSelect = ApplicationResetOrSelect.fromRaw raw
                return raw |> Field.withValue (ApplicationResetOrSelect resetOrSelect)
        }
