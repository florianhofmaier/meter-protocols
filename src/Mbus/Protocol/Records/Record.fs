namespace Metering.Mbus.Protocol.Records

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records.DataInfoBlocks
open Metering.Mbus.Protocol.Records.ValueInfoBlocks

type DataRecordRsp =
    {
        Dib : Dib
        Vib : VibRsp
        Value : Value
    }

type DataRecordCmd =
    {
        Dib : Dib
        Vib : VibCmd
        Value : Value
    }

type MfrSpecificData =
    private MfrSpecificData of ReadOnlyMemory<byte>

module MfrSpecificData =

    let fromRaw bytes =
        MfrSpecificData bytes

    let bytes (MfrSpecificData bytes) =
        bytes

type RecordRsp =
    | Data of DataRecordRsp
    | IdleFiller
    | MfrData of MfrSpecificData
    | MfrDataMoreFollows of MfrSpecificData

type RecordCmd =
    | Data of DataRecordCmd
    | IdleFiller

module DataRecordRsp =

    let fromRaw
        (raw: ParsedField<DataRecordRaw>)
        : Validation<DataRecordRsp> =

        validator {
            let! dib = Dib.fromRaw raw.Value.Dib
            and! vib = VibRsp.fromRaw raw.Value.Vib
            and! value = Value.fromRaw raw.Value.Value

            return
                {
                    Dib = dib
                    Vib = vib
                    Value = value
                }
        }

module DataRecordCmd =

    let fromRaw
        (raw: ParsedField<DataRecordRaw>)
        : Validation<DataRecordCmd> =

        validator {
            let! dib = Dib.fromRaw raw.Value.Dib
            and! vib = VibCmd.fromRaw raw.Value.Vib
            and! value = Value.fromRaw raw.Value.Value

            return
                {
                    Dib = dib
                    Vib = vib
                    Value = value
                }
        }

    let fromRecordRaw
        (raw: RecordRaw)
        : Validation<RecordCmd> =

        validator {
            match raw with
            | RecordRaw.Data data ->
                let! record = fromRaw data
                return RecordCmd.Data record

            | RecordRaw.IdleFiller _ ->
                return RecordCmd.IdleFiller

            | RecordRaw.Selection selection ->
                return! failed selection "Selection records are not implemented in command records"

            | RecordRaw.MfrData data ->
                return! failed data "Manufacturer-specific data is not supported in command records"

            | RecordRaw.MfrDataMoreFollows data ->
                return! failed data "Manufacturer-specific data more-follows is not supported in command records"
        }

module RecordRsp =

    let fromRaw
        (raw: RecordRaw)
        : Validation<RecordRsp> =

        validator {
            match raw with
            | RecordRaw.Data data ->
                let! record = DataRecordRsp.fromRaw data
                return RecordRsp.Data record

            | RecordRaw.IdleFiller _ ->
                return RecordRsp.IdleFiller

            | RecordRaw.MfrData data ->
                return data.Value |> MfrSpecificData.fromRaw |> RecordRsp.MfrData

            | RecordRaw.MfrDataMoreFollows data ->
                return data.Value |> MfrSpecificData.fromRaw |> RecordRsp.MfrDataMoreFollows

            | RecordRaw.Selection selection ->
                return! failed selection "Selection for readout is not valid in response records"
        }
