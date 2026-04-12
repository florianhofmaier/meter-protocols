module Mbus.Records.Tests.Builders.VibBuilderTests

open Mbus
open Mbus.Records
open Mbus.Records.RecordsBuilders
open Mbus.Records.ValueInfoBlocks
open Xunit
open FsUnit.Xunit

let shouldMap expectedValType expectedUnit expectedScaler func =
    let record : RspDataRecord = NoData |> func
    match record.Vib with
    | RspVib.Normal nv ->
        nv.Def.Val |> should equal expectedValType
        nv.Def.Unit |> should equal expectedUnit
        nv.Def.Scaler |> should equal expectedScaler
    | _ -> failwith "Expected Normal Vib"

#region EnergyRecords

[<Fact>]
let ``EnergyRecords.asWattHoursExpMinus3 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExpMinus3
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e-3m

[<Fact>]
let ``EnergyRecords.asWattHoursExpMinus2 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExpMinus2
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e-2m

[<Fact>]
let ``EnergyRecords.asWattHoursExpMinus1 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExpMinus1
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e-1m

[<Fact>]
let ``EnergyRecords.asWattHoursExp0 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp0
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1m

[<Fact>]
let ``EnergyRecords.asWattHoursExp1 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp1
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e1m

[<Fact>]
let ``EnergyRecords.asWattHoursExp2 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp2
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e2m

[<Fact>]
let ``EnergyRecords.asWattHoursExp3 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp3
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e3m

[<Fact>]
let ``EnergyRecords.asWattHoursExp4 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp4
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e4m

[<Fact>]
let ``EnergyRecords.asWattHoursExp5 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp5
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e5m

[<Fact>]
let ``EnergyRecords.asWattHoursExp6 should create a record with correct vib`` () =
    EnergyRecords.asWattHoursExp6
    |> shouldMap MbusValueType.Energy MbusUnit.WattHours 1e6m

#endregion