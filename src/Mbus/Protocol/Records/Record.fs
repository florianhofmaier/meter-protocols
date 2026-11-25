namespace Metering.Mbus.Protocol.Records

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records.DataInfoBlocks
open Metering.Mbus.Protocol.Records.ValueInfoBlocks

type DataRecordRsp = { Dib: Dib; Vib: VibRsp; Value: Value }

type DataRecordCmd = { Dib: Dib; Vib: VibCmd; Value: Value }

type MfrSpecificData = private MfrSpecificData of ReadOnlyMemory<byte>

module MfrSpecificData =

    let ofBytes bytes = MfrSpecificData bytes

    let bytes (MfrSpecificData bytes) = bytes

    /// Writes the unstructured manufacturer bytes without an MDH marker.
    let write data = writeMemory (bytes data)

type RecordRsp =
    | Data of DataRecordRsp
    | IdleFiller
    | MfrData of MfrSpecificData
    | MfrDataMoreFollows of MfrSpecificData

type RecordCmd =
    | Data of DataRecordCmd
    | IdleFiller

module DataRecordRsp =

    let validate (raw: Field<DataRecordRaw>) : Validation<DataRecordRsp> =

        validator {
            let! dib = Dib.validate raw.Value.Dib
            and! vib = VibRsp.validate raw.Value.Vib
            and! value = Value.validate raw.Value.Value

            return { Dib = dib; Vib = vib; Value = value }
        }

    /// Writes a complete data record.
    let write (record: DataRecordRsp) : Writer<unit> =
        writer {
            if record.Value = SelectionForReadout then
                return! writerError "Selection records require a separate selection model"

            do! Dib.write (Value.dataField record.Value) record.Dib
            do! VibRsp.write record.Vib
            do! Value.write record.Value
        }

module DataRecordCmd =

    let validate (raw: Field<DataRecordRaw>) : Validation<DataRecordCmd> =

        validator {
            let! dib = Dib.validate raw.Value.Dib
            and! vib = VibCmd.validate raw.Value.Vib
            and! value = Value.validate raw.Value.Value

            return { Dib = dib; Vib = vib; Value = value }
        }

    let validateRecord (raw: RecordRaw) : Validation<RecordCmd> =

        validator {
            match raw with
            | RecordRaw.Data data ->
                let! record = validate data
                return RecordCmd.Data record

            | RecordRaw.IdleFiller _ -> return RecordCmd.IdleFiller

            | RecordRaw.Selection selection ->
                return! failed selection "Selection records are not implemented in command records"

            | RecordRaw.MfrData data ->
                return! failed data "Manufacturer-specific data is not supported in command records"

            | RecordRaw.MfrDataMoreFollows data ->
                return!
                    failed
                        data
                        "Manufacturer-specific data more-follows is not supported in command records"
        }

    /// Writes a complete data record.
    let write (record: DataRecordCmd) : Writer<unit> =
        writer {
            if record.Value = SelectionForReadout then
                return! writerError "Selection records require a separate selection model"

            do! Dib.write (Value.dataField record.Value) record.Dib
            do! VibCmd.write record.Vib
            do! Value.write record.Value
        }

module RecordRsp =

    let private manufacturerPayload expected (data: Field<ReadOnlyMemory<byte>>) =
        if data.Value.IsEmpty || data.Value.Span[0] <> expected then
            failed data $"Expected manufacturer data header 0x{expected:X2}"
        else
            data.Value.Slice(1) |> MfrSpecificData.ofBytes |> passed

    let validate (raw: RecordRaw) : Validation<RecordRsp> =

        validator {
            match raw with
            | RecordRaw.Data data ->
                let! record = DataRecordRsp.validate data
                return RecordRsp.Data record

            | RecordRaw.IdleFiller _ -> return RecordRsp.IdleFiller

            | RecordRaw.MfrData data ->
                return! manufacturerPayload 0x0Fuy data |> map RecordRsp.MfrData

            | RecordRaw.MfrDataMoreFollows data ->
                return! manufacturerPayload 0x1Fuy data |> map RecordRsp.MfrDataMoreFollows

            | RecordRaw.Selection selection ->
                return! failed selection "Selection for readout is not valid in response records"
        }

    /// Writes a response record, including its special DIF when applicable.
    let write record : Writer<unit> =
        match record with
        | RecordRsp.Data data -> DataRecordRsp.write data
        | RecordRsp.IdleFiller -> writeU8 0x2Fuy
        | RecordRsp.MfrData data ->
            writer {
                do! writeU8 0x0Fuy
                do! MfrSpecificData.write data
            }
        | RecordRsp.MfrDataMoreFollows data ->
            writer {
                do! writeU8 0x1Fuy
                do! MfrSpecificData.write data
            }

module RecordCmd =
    /// Writes a command record.
    let write =
        function
        | RecordCmd.Data data -> DataRecordCmd.write data
        | RecordCmd.IdleFiller -> writeU8 0x2Fuy
