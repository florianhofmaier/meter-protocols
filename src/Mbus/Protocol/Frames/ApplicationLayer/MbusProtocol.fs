namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
module ValidationUtility = Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Records

type RecordsRaw =
    private
        Records of RecordRaw list

module RecordsRaw =

    let toList (Records records) =
        records

    let parse: Parser<Field<RecordsRaw>> =
        parseField
            "APL Data"
            <| parseUntilEnd RecordRaw.parse
        |>> Field.map Records

type RspUdData =
    private
        {
            Records: DataRecordRsp list
            MfrData: MfrSpecificData option
            MoreFollows: bool
        }

module RspUdData =

    let records rspUdData =
        rspUdData.Records

    let mfrData rspUdData =
        rspUdData.MfrData

    let moreFollows rspUdData =
        rspUdData.MoreFollows

    type private Record =
        | Data of DataRecordRsp
        | MfrData of MfrSpecificData
        | MfrDataMoreFollows of MfrSpecificData

    let private withoutIdleFiller record =
        match record with
        | RecordRsp.Data data -> Some (Data data)
        | RecordRsp.IdleFiller -> None
        | RecordRsp.MfrData data -> Some (MfrData data)
        | RecordRsp.MfrDataMoreFollows data -> Some (MfrDataMoreFollows data)

    let private addRecord state record =
        match record with
        | Data data ->
            { state with Records = data :: state.Records }

        | MfrData data ->
            { state with MfrData = Some data }

        | MfrDataMoreFollows data ->
            { state with MfrData = Some data; MoreFollows = true }

    let fromRaw
        (records: RecordsRaw)
        : Validation<RspUdData> =

        validator {
            let! records =
                records
                |> RecordsRaw.toList
                |> ValidationUtility.traverse RecordRsp.fromRaw

            let empty =
                {
                    Records = []
                    MfrData = None
                    MoreFollows = false
                }

            return
                records
                |> List.choose withoutIdleFiller
                |> List.fold addRecord empty
                |> fun data -> { data with Records = List.rev data.Records }
        }

type SndUdData =
    private
        SndUdData of DataRecordCmd list

module SndUdData =

    let records (SndUdData records) =
        records

    let private withoutIdleFiller record =
        match record with
        | RecordCmd.Data data -> Some data
        | RecordCmd.IdleFiller -> None

    let fromRaw
        (records: RecordsRaw)
        : Validation<SndUdData> =

        validator {
            let! records =
                records
                |> RecordsRaw.toList
                |> ValidationUtility.traverse DataRecordCmd.fromRecordRaw

            return
                records
                |> List.choose withoutIdleFiller
                |> SndUdData
        }
