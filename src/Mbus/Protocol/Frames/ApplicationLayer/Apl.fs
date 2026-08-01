namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.Protection

type AplParsedRaw =
    | RspUdData of Field<RecordsRaw>
    | AlarmBits of Field<Alarms>
    | SndUdData of Field<RecordsRaw>
    | ApplicationResetOrSelect of Field<ApplicationResetOrSelectRaw>

type AplProtectedRaw =
    {
        Bytes: Field<ReadOnlyMemory<byte>>
        Failure: UnprotectionIssue
    }

type AplRaw =
    | Parsed of AplParsedRaw
    | Protected of AplProtectedRaw

module AplRaw =

    let parse
        (ci : CiFieldTpl)
        : Parser<Field<AplParsedRaw>> =

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
        (raw: Field<AplRaw>)
        : Validation<Field<Apl>> =

        validator {
            match raw.Value with
            | AplRaw.RspUdData records ->
                let! data = RspUdData.fromRaw records.Value
                return raw |> Field.withValue (RspUdData data)

            | AplRaw.SndUdData records ->
                let! data = SndUdData.fromRaw records.Value
                return raw |> Field.withValue (SndUdData data)

            | AplRaw.AlarmBits alarms ->
                return raw |> Field.withValue (AlarmBits alarms.Value)

            | AplRaw.ApplicationResetOrSelect aplRaw ->
                let! resetOrSelect = ApplicationResetOrSelect.fromRaw aplRaw
                return
                    raw
                    |> Field.withValue (
                        ApplicationResetOrSelect resetOrSelect.Value
                    )
        }
