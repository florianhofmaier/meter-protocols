namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type AplRaw =
    | RspUdData of Field<RecordsRaw>
    | AlarmBits of Field<Alarms>
    | SndUdData of Field<RecordsRaw>
    | ApplicationResetOrSelect of Field<ApplicationResetOrSelectRaw>

module AplRaw =

    let parse
        (ci : CiFieldTpl)
        : Parser<Field<AplRaw>> =

        parseField "APL"
        <| parser {
            match ci with
            | CiFieldTpl.NoneHeader noneCi ->
                match noneCi with
                | CiFieldTplNoneHeader.ApplicationResetOrSelect ->
                    return!
                        ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplNoneHeader.Command ->
                    return! RecordsRaw.parse |>> SndUdData

            | CiFieldTpl.ShortHeader shortCi ->
                match shortCi with
                | CiFieldTplShortHeader.ApplicationResetOrSelect ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplShortHeader.Response ->
                    return! RecordsRaw.parse |>> RspUdData

            | CiFieldTpl.LongHeader longCi ->
                match longCi with
                | CiFieldTplLongHeader.ApplicationResetOrSelect ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplLongHeader.Response ->
                    return! RecordsRaw.parse |>> RspUdData

                | CiFieldTplLongHeader.Alarm ->
                    return! Alarms.parse |>> AlarmBits
        }

type Apl =
    | RspUdData of RspUdData
    | AlarmBits of Alarms
    | SndUdData of SndUdData
    | ApplicationResetOrSelect of ApplicationResetOrSelect

module Apl =

    let fromRaw
        (raw: AplRaw)
        : Validation<Apl> =

        validator {
            match raw with
            | AplRaw.RspUdData data ->
                return! data.Value |> RspUdData.fromRaw |> map RspUdData
            | AplRaw.SndUdData data ->
                return! data.Value |> SndUdData.fromRaw |> map SndUdData
            | AplRaw.AlarmBits data ->
                return AlarmBits data.Value
            | AplRaw.ApplicationResetOrSelect data ->
                return! data |> ApplicationResetOrSelect.fromRaw |> map ApplicationResetOrSelect
        }
