module Mbus.Messages.Tests.RecordBuildersTests
//
// open System
// open Mbus
// open Mbus.BaseWriters.Core
// open Mbus.Messages
// open Mbus.Records
// open Mbus.Records.DataInfoBlocks
// open Mbus.Records.VifDef
// open Xunit
// open FsUnit.Xunit
//
// // Helper to build and serialize a record
// let buildRecord record =
//     let writer = RecordsBuilders.build record
//     let wState = WState.create()
//     match writer wState with
//     | Ok (_, ws) -> Ok (ws.Buf.AsMemory(0, ws.Pos).ToArray())
//     | Error e -> Error e
//
// // Helper to create a record and check it builds successfully
// let assertBuilds record =
//     match buildRecord record with
//     | Ok _ -> ()
//     | Error e -> failwithf $"Expected record to build, but got error: {e}"
//
// // ===== Values Module Tests =====
//
// [<Fact>]
// let ``withInt32 should create Int32 value`` () =
//     let result = RecordsBuilders.Values.withInt32 42
//     result |> should equal (Ok (Int32 42))
//
// [<Fact>]
// let ``withInt32 with negative value should succeed`` () =
//     let result = RecordsBuilders.Values.withInt32 -123
//     result |> should equal (Ok (Int32 -123))
//
// [<Fact>]
// let ``withBcd8Digit with valid value should succeed`` () =
//     let result = RecordsBuilders.Values.withBcd8Digit 12345678
//     result |> should equal (Ok (Bcd8Digit 12345678u))
//
// [<Fact>]
// let ``withBcd8Digit with zero should succeed`` () =
//     let result = RecordsBuilders.Values.withBcd8Digit 0
//     result |> should equal (Ok (Bcd8Digit 0u))
//
// [<Fact>]
// let ``withBcd8Digit with maximum valid value should succeed`` () =
//     let result = RecordsBuilders.Values.withBcd8Digit 99999999
//     result |> should equal (Ok (Bcd8Digit 99999999u))
//
// [<Fact>]
// let ``withBcd8Digit with negative value should return error`` () =
//     let result = RecordsBuilders.Values.withBcd8Digit -1
//     match result with
//     | Error msg -> msg |> should contain "out of"
//     | Ok _ -> failwith "Expected error for negative value"
//
// [<Fact>]
// let ``withBcd8Digit with value exceeding max should return error`` () =
//     let result = RecordsBuilders.Values.withBcd8Digit 100000000
//     match result with
//     | Error msg -> msg |> should contain "out of"
//     | Ok _ -> failwith "Expected error for value exceeding max"
//
// // ===== EnergyRecords Module Tests =====
//
// let assertVib record expectedValType expectedUnit expectedScaler =
//     match record.Vib with
//     | Normal nv ->
//         nv.Def.Val |> should equal expectedValType
//         nv.Def.Unit |> should equal expectedUnit
//         nv.Def.Scaler |> should equal expectedScaler
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asWattHours with ExpMinus3 should create correct record`` () =
//     let record = NoData |> RecordsBuilders.EnergyRecords.asWattHoursExpMinus3
//     assertVib record MbusValueType.Energy MbusUnit.WattHours 1e-3m
//
// [<Fact>]
//
// [<Fact>]
// let ``asWattHours with Exp0 should create correct record`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 42)
//     record.Value |> should equal (Int32 42)
//     record.Fn |> should equal MbusFunctionField.InstValue
//     StorageNumber.value record.StNum |> should equal 0
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x03uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asWattHours with Exp1 should map to correct VIF`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp1 (Int32 123)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x04uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asWattHours with Exp4 should map to correct VIF`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp4 (Int32 999)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x07uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asWattHours with Exp5 should map to first extension table`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp5 (Int32 5000)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal firstExtTable[0x00uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asWattHours with Exp6 should map to first extension table`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp6 (Int32 6000)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal firstExtTable[0x01uy]
//     | _ -> failwith "Expected Normal Vib"
//
// // ===== VolumeRecords Module Tests =====
//
// [<Fact>]
// let ``asCubicMeters with ExpMinus6 should create correct record`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.ExpMinus6 (Int32 1234)
//     record.Value |> should equal (Int32 1234)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x10uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asCubicMeters with ExpMinus3 should create correct record`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.ExpMinus3 (Int32 5678)
//     record.Value |> should equal (Int32 5678)
//     record.Fn |> should equal MbusFunctionField.InstValue
//     StorageNumber.value record.StNum |> should equal 0
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x13uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asCubicMeters with Exp0 should map to correct VIF`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 100)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x16uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asCubicMeters with Exp1 should map to correct VIF`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp1 (Int32 500)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x17uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asCubicMeters with Exp2 should map to first extension table`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp2 (Int32 2000)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal firstExtTable[0x10uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asCubicMeters with Exp3 should map to first extension table`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp3 (Int32 3000)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal firstExtTable[0x11uy]
//     | _ -> failwith "Expected Normal Vib"
//
// // ===== FabricationNumbers Module Tests =====
//
// [<Fact>]
// let ``asFabNum should create correct record`` () =
//     let record = RecordsBuilders.FabricationNumbers.asFabNum (Bcd8Digit 12345678u)
//     record.Value |> should equal (Bcd8Digit 12345678u)
//     match record.Vib with
//     | Vib.Normal nv -> nv.Def |> should equal primTable[0x78uy]
//     | _ -> failwith "Expected Normal Vib"
//
// [<Fact>]
// let ``asFabNum should have correct defaults`` () =
//     let record = RecordsBuilders.FabricationNumbers.asFabNum (Bcd8Digit 11111111u)
//     record.Fn |> should equal MbusFunctionField.InstValue
//     StorageNumber.value record.StNum |> should equal 0
//     Tariff.value record.Tariff |> should equal 0
//     SubUnit.value record.SubUnit |> should equal 0
//
// // ===== Modifier Functions Tests =====
//
// [<Fact>]
// let ``withFunction should change function field`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 100)
//     let result = RecordsBuilders.withFunction MbusFunctionField.MaxValue record
//     match result with
//     | Ok r -> r.Fn |> should equal MbusFunctionField.MaxValue
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withFunction should preserve other fields`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.ExpMinus3 (Int32 456)
//     let result = RecordsBuilders.withFunction MbusFunctionField.MinValue record
//     match result with
//     | Ok r ->
//         r.Value |> should equal (Int32 456)
//         StorageNumber.value r.StNum |> should equal 0
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withStNum with valid value should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 200)
//     let result = RecordsBuilders.withStNum 5 record
//     match result with
//     | Ok r -> StorageNumber.value r.StNum |> should equal 5
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withStNum with zero should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 300)
//     let result = RecordsBuilders.withStNum 0 record
//     match result with
//     | Ok r -> StorageNumber.value r.StNum |> should equal 0
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withStNum with maximum value should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 400)
//     let result = RecordsBuilders.withStNum 15 record
//     match result with
//     | Ok r -> StorageNumber.value r.StNum |> should equal 15
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withStNum with negative value should return error`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 500)
//     let result = RecordsBuilders.withStNum -1 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for negative storage number"
//
// [<Fact>]
// let ``withStNum with value exceeding max should return error`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 600)
//     let result = RecordsBuilders.withStNum 16 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for storage number exceeding max"
//
// [<Fact>]
// let ``withTariff with valid value should succeed`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 700)
//     let result = RecordsBuilders.withTariff 2 record
//     match result with
//     | Ok r -> Tariff.value r.Tariff |> should equal 2
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withTariff with zero should succeed`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 800)
//     let result = RecordsBuilders.withTariff 0 record
//     match result with
//     | Ok r -> Tariff.value r.Tariff |> should equal 0
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withTariff with maximum value should succeed`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 900)
//     let result = RecordsBuilders.withTariff 1048575 record
//     match result with
//     | Ok r -> Tariff.value r.Tariff |> should equal 3
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withTariff with negative value should return error`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 1000)
//     let result = RecordsBuilders.withTariff -1 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for negative tariff"
//
// [<Fact>]
// let ``withTariff with value exceeding max should return error`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.Exp0 (Int32 1100)
//     let result = RecordsBuilders.withTariff 1048576 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for tariff exceeding max"
//
// [<Fact>]
// let ``withSubUnit with valid value should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1200)
//     let result = RecordsBuilders.withSubUnit 1 record
//     match result with
//     | Ok r -> SubUnit.value r.SubUnit |> should equal 1
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withSubUnit with zero should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1300)
//     let result = RecordsBuilders.withSubUnit 0 record
//     match result with
//     | Ok r -> SubUnit.value r.SubUnit |> should equal 0
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withSubUnit with maximum value should succeed`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1400)
//     let result = RecordsBuilders.withSubUnit 15 record
//     match result with
//     | Ok r -> SubUnit.value r.SubUnit |> should equal 15
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withSubUnit with negative value should return error`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1500)
//     let result = RecordsBuilders.withSubUnit -1 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for negative subunit"
//
// [<Fact>]
// let ``withSubUnit with value exceeding max should return error`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1600)
//     let result = RecordsBuilders.withSubUnit 16 record
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error for subunit exceeding max"
//
// [<Fact>]
// let ``withVibExtension should add extension to normal Vib`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1700)
//     let result = RecordsBuilders.withVibExtension (ValidCombVifExt MbusValueTypeExtension.PerSecond) record
//     match result with
//     | Ok r ->
//         match r.Vib with
//         | Vib.Normal nv ->
//             nv.Ext |> should haveLength 1
//             nv.Ext[0] |> should equal (ValidCombVifExt MbusValueTypeExtension.PerSecond)
//         | _ -> failwith "Expected Normal Vib"
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``withVibExtension should add multiple extensions`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1800)
//     let result =
//         record
//         |> RecordsBuilders.withVibExtension (ValidCombVifExt MbusValueTypeExtension.PerHour)
//         |> Result.bind (RecordsBuilders.withVibExtension (ValidCombVifExt MbusValueTypeExtension.PerMinute))
//     match result with
//     | Ok r ->
//         match r.Vib with
//         | Vib.Normal nv ->
//             nv.Ext |> should haveLength 2
//         | _ -> failwith "Expected Normal Vib"
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// // ===== Integration Tests =====
//
// [<Fact>]
// let ``build should serialize simple energy record`` () =
//     let record = RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0 (Int32 1000)
//     match buildRecord record with
//     | Ok bytes -> bytes.Length |> should be (greaterThan 0)
//     | Error e -> failwithf $"Build failed: {e}"
//
// [<Fact>]
// let ``build should serialize simple volume record`` () =
//     let record = RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.ExpMinus3 (Int32 5000)
//     match buildRecord record with
//     | Ok bytes -> bytes.Length |> should be (greaterThan 0)
//     | Error e -> failwithf $"Build failed: {e}"
//
// [<Fact>]
// let ``chaining modifiers should work correctly`` () =
//     let result =
//         42
//         |> RecordsBuilders.Values.withInt32
//         |> Result.map (RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0)
//         |> Result.bind (RecordsBuilders.withStNum 1)
//         |> Result.bind (RecordsBuilders.withTariff 2)
//         |> Result.bind (RecordsBuilders.withFunction MbusFunctionField.MaxValue)
//
//     match result with
//     | Ok record ->
//         record.Value |> should equal (Int32 42)
//         StorageNumber.value record.StNum |> should equal 1
//         Tariff.value record.Tariff |> should equal 2
//         record.Fn |> should equal MbusFunctionField.MaxValue
//     | Error e -> failwithf $"Expected Ok, got Error: {e}"
//
// [<Fact>]
// let ``chaining modifiers with invalid value should propagate error`` () =
//     let result =
//         42
//         |> RecordsBuilders.Values.withInt32
//         |> Result.map (RecordsBuilders.EnergyRecords.asWattHours WattHoursScaler.Exp0)
//         |> Result.bind (RecordsBuilders.withStNum 99) // Invalid
//
//     match result with
//     | Error msg -> msg |> should contain "out of range"
//     | Ok _ -> failwith "Expected error to propagate"
//
// [<Fact>]
// let ``complete example with build should succeed`` () =
//     let result =
//         12345
//         |> RecordsBuilders.Values.withInt32
//         |> Result.map (RecordsBuilders.VolumeRecords.asCubicMeters CubicMetersScaler.ExpMinus3)
//         |> Result.bind (RecordsBuilders.withStNum 5)
//
//     match result with
//     | Ok record ->
//         let writer = RecordsBuilders.build record
//         let wState = WState.create()
//         match writer wState with
//         | Ok _ -> () // Success
//         | Error e -> failwithf $"Writer execution failed: {e}"
//     | Error e -> failwithf $"Record creation failed: {e}"
//
// [<Fact>]
// let ``testBuild example should compile and run`` () =
//     match RecordsBuilders.testBuild with
//     | Ok writer ->
//         let wState = WState.create()
//         match writer wState with
//         | Ok _ -> () // Success
//         | Error e -> failwithf $"Writer execution failed: {e}"
//     | Error e -> failwithf $"Test build failed: {e}"
//
