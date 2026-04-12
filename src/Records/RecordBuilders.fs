module Mbus.Records.RecordsBuilders

open Mbus
open Mbus.BaseWriters.Core
open Mbus.Records
open Mbus.Records.DataInfoBlocks
open Mbus.Records.ValueInfoBlocks

let createDataRecord vifDef value : RspDataRecord =
    {
        Value = value
        StNum = StorageNumber.zero
        Fn = MbusFunctionField.InstValue
        Tariff = Tariff.zero
        SubUnit = SubUnit.zero
        Vib = RspVib.Normal { Def = vifDef; Ext = []; Codes = [] }
    }

let withFunction fn (record: RspDataRecord) =
    Ok { record with Fn = fn }

let withStNum n (record: RspDataRecord) =
    StorageNumber.create n
    |> Result.map (fun stNum -> { record with StNum = stNum })

let withTariff tariff (record: RspDataRecord) =
    Tariff.create tariff
    |> Result.map (fun t -> { record with Tariff = t })

let withSubUnit subUnit (record: RspDataRecord) =
    SubUnit.create subUnit
    |> Result.map (fun su -> { record with SubUnit = su })

let withVibExtension ext (record: RspDataRecord) =
    match record.Vib with
    | RspVib.Normal vib -> Ok { record with Vib = RspVib.Normal { vib with Ext = vib.Ext @ [ ext ] } }
    | _ -> Error "Cannot add Vib extension to non-normal Vib"

let build (record: RspDataRecord) : Writer<unit> =
    Record.Writer.write record

module EnergyRecords =
    let asWattHoursExpMinus3 value =
        createDataRecord VifDef.primTable[0x00uy] value

    let asWattHoursExpMinus2 value =
        createDataRecord VifDef.primTable[0x01uy] value

    let asWattHoursExpMinus1 value =
        createDataRecord VifDef.primTable[0x02uy] value

    let asWattHoursExp0 value =
        createDataRecord VifDef.primTable[0x03uy] value

    let asWattHoursExp1 value =
        createDataRecord VifDef.primTable[0x04uy] value

    let asWattHoursExp2 value =
        createDataRecord VifDef.primTable[0x05uy] value

    let asWattHoursExp3 value =
        createDataRecord VifDef.primTable[0x06uy] value

    let asWattHoursExp4 value =
        createDataRecord VifDef.primTable[0x07uy] value

    let asWattHoursExp5 value =
        createDataRecord VifDef.firstExtTable[0x00uy] value

    let asWattHoursExp6 value =
        createDataRecord VifDef.firstExtTable[0x01uy] value

module VolumeRecords =
    let asCubicMetersExpMinus6 value =
        createDataRecord VifDef.primTable[0x10uy] value

    let asCubicMetersExpMinus5 value =
        createDataRecord VifDef.primTable[0x11uy] value

    let asCubicMetersExpMinus4 value =
        createDataRecord VifDef.primTable[0x12uy] value

    let asCubicMetersExpMinus3 value =
        createDataRecord VifDef.primTable[0x13uy] value

    let asCubicMetersExpMinus2 value =
        createDataRecord VifDef.primTable[0x14uy] value

    let asCubicMetersExpMinus1 value =
        createDataRecord VifDef.primTable[0x15uy] value

    let asCubicMetersExp0 value =
        createDataRecord VifDef.primTable[0x16uy] value

    let asCubicMetersExp1 value =
        createDataRecord VifDef.primTable[0x17uy] value

    let asCubicMetersExp2 value =
        createDataRecord VifDef.firstExtTable[0x10uy] value

    let asCubicMetersExp3 value =
        createDataRecord VifDef.firstExtTable[0x11uy] value

module FabricationNumbers =
    let asFabNum value =
        createDataRecord VifDef.primTable[0x78uy] value

module Values =

    let private guardedValue value min max typeName =
        if value >= min && value <= max then
            Ok value
        else
            Error $"Value {value} is out of {typeName} range [{min}..{max}]"

    let withInt32 n =
        Int32 n |> Ok

    let withBcd8Digit n =
        guardedValue n 0 99999999 "Bcd8Digit"
        |> Result.map (fun v -> v |> uint32 |> Bcd8Digit)

let testBuild =
    42
    |> Values.withInt32
    |> Result.map EnergyRecords.asWattHoursExp0
    |> Result.bind (withStNum 1)
    |> Result.map build
