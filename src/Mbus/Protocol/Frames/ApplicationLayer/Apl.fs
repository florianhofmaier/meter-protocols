namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames

type AplParsedRaw =
    | RspUdData of Field<RecordsRaw>
    | AlarmBits of Field<Alarms>
    | SndUdData of Field<RecordsRaw>
    | ApplicationResetOrSelect of Field<ApplicationResetOrSelectRaw>

type UnprotectionError =
    | Encryption of EncryptionError
    | Validation of Failures

type AplProtectedRaw =
    {
        Bytes: Field<ReadOnlyMemory<byte>>
        Error: UnprotectionError
    }

type AplRaw =
    | Parsed of AplParsedRaw
    | Protected of Field<AplProtectedRaw>

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
    | Protected of AplProtectedRaw

module Apl =

    let fromRaw
        (raw: AplRaw)
        : Validation<Apl> =

        validator {
            match raw with
            | AplRaw.Protected bytes ->
                return Protected bytes.Value

            | Parsed apl ->
                match apl with
                | AplParsedRaw.RspUdData data ->
                    return!
                        data.Value
                        |> RspUdData.fromRaw
                        |> map RspUdData

                | AplParsedRaw.SndUdData data ->
                    return!
                        data.Value
                        |> SndUdData.fromRaw
                        |> map SndUdData

                | AplParsedRaw.AlarmBits data ->
                    return AlarmBits data.Value

                | AplParsedRaw.ApplicationResetOrSelect data ->
                    return!
                        data
                        |> ApplicationResetOrSelect.fromRaw
                        |> map ApplicationResetOrSelect
        }
