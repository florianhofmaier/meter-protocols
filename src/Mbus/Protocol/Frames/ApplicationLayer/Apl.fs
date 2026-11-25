namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type AplRaw =
    | RequestSelectedApplication of Field<System.ReadOnlyMemory<byte>>
    | SelectedApplicationResponse of Field<System.ReadOnlyMemory<byte>>
    | RspUdData of Field<RecordsRaw>
    | AlarmBits of Field<Alarms>
    | SndUdData of Field<RecordsRaw>
    | ApplicationResetOrSelect of Field<ApplicationResetOrSelectRaw>

module AplRaw =

    let parse (ci: CiFieldTpl) : Parser<Field<AplRaw>> =

        parseField "APL"
        <| parser {
            match ci with
            | CiFieldTpl.NoneHeader noneCi ->
                match noneCi with
                | CiFieldTplNoneHeader.RequestSelectedApplication ->
                    return!
                        parseField
                            "Selected application data"
                            Metering.Common.Decoding.Parsers.Utility.takeAll
                        |>> RequestSelectedApplication
                | CiFieldTplNoneHeader.SelectedApplicationResponse ->
                    return!
                        parseField
                            "Selected application data"
                            Metering.Common.Decoding.Parsers.Utility.takeAll
                        |>> SelectedApplicationResponse
                | CiFieldTplNoneHeader.ApplicationResetOrSelect ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplNoneHeader.Command -> return! RecordsRaw.parse |>> SndUdData

            | CiFieldTpl.ShortHeader shortCi ->
                match shortCi with
                | CiFieldTplShortHeader.RequestSelectedApplication ->
                    return!
                        parseField
                            "Selected application data"
                            Metering.Common.Decoding.Parsers.Utility.takeAll
                        |>> RequestSelectedApplication
                | CiFieldTplShortHeader.ApplicationResetOrSelect ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplShortHeader.Response -> return! RecordsRaw.parse |>> RspUdData

            | CiFieldTpl.LongHeader longCi ->
                match longCi with
                | CiFieldTplLongHeader.RequestSelectedApplication ->
                    return!
                        parseField
                            "Selected application data"
                            Metering.Common.Decoding.Parsers.Utility.takeAll
                        |>> RequestSelectedApplication
                | CiFieldTplLongHeader.SelectedApplicationResponse ->
                    return!
                        parseField
                            "Selected application data"
                            Metering.Common.Decoding.Parsers.Utility.takeAll
                        |>> SelectedApplicationResponse
                | CiFieldTplLongHeader.Command -> return! RecordsRaw.parse |>> SndUdData

                | CiFieldTplLongHeader.ApplicationResetOrSelect ->
                    return! ApplicationResetOrSelectRaw.parse |>> ApplicationResetOrSelect

                | CiFieldTplLongHeader.Response -> return! RecordsRaw.parse |>> RspUdData

                | CiFieldTplLongHeader.Alarm -> return! Alarms.parse |>> AlarmBits
        }

type Apl =
    | RequestSelectedApplication
    | SelectedApplicationResponse of System.ReadOnlyMemory<byte>
    | RspUdData of RspUdData
    | AlarmBits of Alarms
    | SndUdData of SndUdData
    | ApplicationResetOrSelect of ApplicationResetOrSelect

module Apl =

    let validate (raw: AplRaw) : Validation<Apl> =

        validator {
            match raw with
            | AplRaw.RequestSelectedApplication data ->
                let! () =
                    ensure
                        data
                        "Request of selected application must not contain parameters"
                        data.Value.IsEmpty

                return RequestSelectedApplication
            | AplRaw.SelectedApplicationResponse data ->
                return SelectedApplicationResponse data.Value
            | AplRaw.RspUdData data -> return! data.Value |> RspUdData.validate |> map RspUdData
            | AplRaw.SndUdData data -> return! data.Value |> SndUdData.validate |> map SndUdData
            | AplRaw.AlarmBits data -> return AlarmBits data.Value
            | AplRaw.ApplicationResetOrSelect data ->
                return! data |> ApplicationResetOrSelect.validate |> map ApplicationResetOrSelect
        }

    /// Writes the application payload represented by this variant.
    let write =
        function
        | RequestSelectedApplication -> Metering.Common.BaseWriters.Core.Writer.ret ()
        | SelectedApplicationResponse bytes ->
            Metering.Common.BaseWriters.BinaryWriters.writeMemory bytes
        | RspUdData data -> RspUdData.write data
        | SndUdData data -> SndUdData.write data
        | AlarmBits data -> Alarms.write data
        | ApplicationResetOrSelect data -> ApplicationResetOrSelect.write data
