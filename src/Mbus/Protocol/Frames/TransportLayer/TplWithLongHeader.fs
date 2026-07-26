namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer

type AplDataLongHeaderRaw =
    | Response of RecordsRaw
    | Alarm of Alarms

type TplWithLongHeaderRaw =
    {
        Ci: Field<Unit>
        Header: Field<LongHeaderRaw>
        AplData: Field<AplDataLongHeaderRaw>
    }

module TplWithLongHeaderRaw =

    let parse ci : Parser<TplWithLongHeaderRaw> =
        parser {
            let! header = LongHeaderRaw.parse

            match ci.Value with
            | CiFieldTplLongHeader.Alarm ->
                let! aplData =
                    Alarms.parse
                    |>> Field.map AplDataLongHeaderRaw.Alarm

                return {
                    Ci = Field.withValue ci ()
                    Header = header
                    AplData = aplData
                }

            | CiFieldTplLongHeader.Response ->
                let! aplData =
                    RecordsRaw.parse
                    |>> Field.map AplDataLongHeaderRaw.Response

                return {
                    Ci = Field.withValue ci ()
                    Header = header
                    AplData = aplData
                }
        }

type AplDataLongHeader =
    | ApplicationResetOrSelect of ApplicationResetOrSelect
    | Response of RspUdData
    | Alarm of Alarms

type TplWithLongHeader =
    {
        Ci: Field<Unit>
        Header: Field<LongHeader>
        AplData: Field<AplDataLongHeader>
    }

module TplWithLongHeader =

    let fromRaw
        (raw: TplWithLongHeaderRaw)
        : Validation<TplWithLongHeader> =

        validator {
            let! header = LongHeader.fromRaw raw.Header

            match raw.AplData.Value with
            | AplDataLongHeaderRaw.Alarm rawAplData ->
                let aplData =
                    rawAplData
                    |> Field.withValue raw.AplData
                    |> Field.map AplDataLongHeader.Alarm

                return {
                    Ci = raw.Ci
                    Header = header
                    AplData = aplData
                }

            | AplDataLongHeaderRaw.Response rawAplData ->
                let! aplData =
                    rawAplData
                    |> RspUdData.fromRaw
                    |> map AplDataLongHeader.Response
                    |> map (Field.withValue raw.AplData)

                return {
                    Ci = raw.Ci
                    Header = header
                    AplData = aplData
                }
        }
