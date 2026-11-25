namespace Metering.Mbus.Devices

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.Utility.Result
open Metering.Mbus
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.DataInfoBlocks
open Metering.Mbus.Protocol.Records.ValueInfoBlocks

/// A fixed data set split into complete response blocks; no link or stream state.
module Readout =

    let payloadCapacity = LinkUserDataBytes.maxLength - 1 - LongHeader.mode0Length

    let blockIdentification index : DataRecordRsp = {
        Dib = {
            Fn = FunctionField.Inst
            StNum = StorageNumber.zero
            Tariff = Tariff.zero
            SubUnit = SubUnit.zero
        }
        Vib =
            VibRsp.Normal {
                Def = {
                    Val = MbusValueType.CurrentSelectedApplication
                    Unit = MbusUnit.NoUnit
                    Scaler = 1m
                }
                Ext = []
                Codes = []
            }
        Value =
            Value.Binary(
                ReadOnlyMemory [|
                    byte (index &&& 15)
                    byte ((index >>> 4) &&& 15)
                    byte ((index >>> 8) &&& 15)
                |]
            )
    }

    /// EN 13757-3:2025, 6.6 and 7.6: never split a record; identify every block in a multi-block response.
    let prepare selectedApplication data =
        let available = payloadCapacity - (if selectedApplication then 3 else 0)

        result {
            let! sizes =
                RspUdData.records data
                |> List.map (fun record ->
                    UserData.encode [ record ]
                    |> Result.map (fun bytes -> record, bytes.Length)
                    |> Result.mapError (fun error -> error.Msg))
                |> List.fold
                    (fun state item ->
                        result {
                            let! items = state
                            let! item = item
                            return item :: items
                        })
                    (Ok [])
                |> Result.map List.rev

            let manufacturer = RspUdData.mfrData data

            let manufacturerSize =
                manufacturer
                |> Option.map (fun bytes -> 1 + (MfrSpecificData.bytes bytes).Length)
                |> Option.defaultValue 0

            let total = (sizes |> List.sumBy snd) + manufacturerSize

            if total <= available then
                return [ RspUdData.create (RspUdData.records data) manufacturer false ]
            else
                let! identification =
                    UserData.encode [ blockIdentification 0 ]
                    |> Result.mapError (fun error -> error.Msg)

                let capacity = available - identification.Length

                let! () =
                    if manufacturerSize > capacity then
                        Error "The unstructured manufacturer block does not fit in a response"
                    else
                        Ok()

                let rec split index current length remaining =
                    result {
                        let! () =
                            if index > 4095 then
                                Error "The readout exceeds the twelve-bit block number range"
                            else
                                Ok()

                        match remaining with
                        | (record, size) :: rest ->
                            let required =
                                size + (if rest.IsEmpty && manufacturerSize = 0 then 0 else 1)

                            if required > capacity then
                                return!
                                    Error
                                        "An individual record does not fit in a response with its continuation marker"
                            elif length + required <= capacity then
                                return! split index (record :: current) (length + size) rest
                            else
                                let block =
                                    RspUdData.create
                                        (blockIdentification index :: List.rev current)
                                        None
                                        true

                                let! following = split (index + 1) [] 0 remaining
                                return block :: following
                        | [] when length + manufacturerSize <= capacity ->
                            return [
                                RspUdData.create
                                    (blockIdentification index :: List.rev current)
                                    manufacturer
                                    false
                            ]
                        | [] ->
                            let block =
                                RspUdData.create
                                    (blockIdentification index :: List.rev current)
                                    None
                                    true

                            return [
                                block
                                RspUdData.create
                                    [ blockIdentification (index + 1) ]
                                    manufacturer
                                    false
                            ]
                    }

                return! split 0 [] 0 sizes
        }
