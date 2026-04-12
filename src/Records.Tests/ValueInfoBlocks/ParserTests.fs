module Mbus.Records.Tests.ValueInfoBlocks.ParserTests

open Mbus
open Mbus.BaseParsers.Core
open Mbus.Records.ValueInfoBlocks
open System
open FsUnit.Xunit
open Mbus.Records.ValueInfoBlocks.VifDef
open Xunit

let parseVib buf =
    match Vib.Parser.parseRsp { Off = 0; Buf = ReadOnlyMemory<uint8>(buf) } with
    | Ok (v, _) -> v
    | Error e -> failwithf $"Unexpected: %A{e}"

let parseVibExpectError buf =
    match Vib.Parser.parseRsp { Off = 0; Buf = ReadOnlyMemory<uint8>(buf) } with
    | Ok (v, _) -> failwithf $"Unexpected: %A{v}"
    | Error e -> e

let shouldBeNormal expectedValueType expectedUnit expectedScaler vib =
    vib |> should equal
        (RspVib.Normal { Def = { Val = expectedValueType; Unit = expectedUnit; Scaler = expectedScaler }; Ext = [ ]; Codes = [] })

let shouldBeText expectedText vib =
    match vib with
    | RspVib.Text t -> t |> should equal expectedText
    | other -> failwithf $"Expected Text, got %A{other}"

let shouldBeInvalid expectedBytes vib =
    match vib with
    | RspVib.Invalid mem -> mem.Span.ToArray() |> should equal expectedBytes
    | other -> failwithf $"Expected Invalid, got %A{other}"

let shouldBeMfrSpecific expectedBytes vib =
    match vib with
    | RspVib.Mfr mem -> mem.Span.ToArray() |> should equal expectedBytes
    | other -> failwithf $"Expected MfrSpecific, got %A{other}"

let shouldHaveExtensions expectedExtensions vib =
    match vib with
    | RspVib.Normal { Ext = ext } -> ext |> should equal expectedExtensions
    | other -> failwithf $"Expected Normal, got %A{other}"

[<Fact>]
let ``parse vif 0x00 expect energy in Wh with scaler 1e-3`` () =
    [| 0x00uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e-3m

[<Fact>]
let ``parse vif 0x01 expect energy in Wh with scaler 1e-2`` () =
    [| 0x01uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e-2m

[<Fact>]
let ``parse vif 0x02 expect energy in Wh with scaler 1e-1`` () =
    [| 0x02uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e-1m

[<Fact>]
let ``parse vif 0x03 expect energy in Wh with scaler 1e0`` () =
    [| 0x03uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e0m

[<Fact>]
let ``parse vif 0x04 expect energy in Wh with scaler 1e1`` () =
    [| 0x04uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e1m

[<Fact>]
let ``parse vif 0x05 expect energy Wh with scaler 1e2`` () =
    [| 0x05uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e2m

[<Fact>]
let ``parse vif 0x06 expect energy in Wh with scaler 1e3`` () =
    [| 0x06uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e3m

[<Fact>]
let ``parse vif 0x07 expect energy in Wh with scaler 1e4`` () =
    [| 0x07uy |]
    |> parseVib
    |> shouldBeNormal Energy WattHours 1e4m

[<Fact>]
let ``parse vif 0x08 expect energy in J with scaler 1e0`` () =
    [| 0x08uy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e0m

[<Fact>]
let ``parse vif 0x09 expect energy in J with scaler 1e1`` () =
    [| 0x09uy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e1m

[<Fact>]
let ``parse vif 0x0A expect energy in J with scaler 1e2`` () =
    [| 0x0Auy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e2m

[<Fact>]
let ``parse vif 0x0B expect energy in J with scaler 1e3`` () =
    [| 0x0Buy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e3m

[<Fact>]
let ``parse vif 0x0C expect energy in J with scaler 1e4`` () =
    [| 0x0Cuy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e4m

[<Fact>]
let ``parse vif 0x0D expect energy in J with scaler 1e5`` () =
    [| 0x0Duy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e5m

[<Fact>]
let ``parse vif 0x0E expect energy in J with scaler 1e6`` () =
    [| 0x0Euy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e6m

[<Fact>]
let ``parse vif 0x0F expect energy in J with scaler 1e7`` () =
    [| 0x0Fuy |]
    |> parseVib
    |> shouldBeNormal Energy Joules 1e7m

[<Fact>]
let ``parse vif 0x10 expect volume in m3 with scaler 1e-6`` () =
    [| 0x10uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-6m

[<Fact>]
let ``parse vif 0x11 expect volume m3 with scaler 1e-5`` () =
    [| 0x11uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-5m

[<Fact>]
let ``parse vif 0x12 expect volume in m3 with scaler 1e-4`` () =
    [| 0x12uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-4m

[<Fact>]
let ``parse vif 0x13 expect volume in m3 with scaler 1e-3`` () =
    [| 0x13uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-3m

[<Fact>]
let ``parse vif 0x14 expect volume in m3 with scaler 1e-2`` () =
    [| 0x14uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-2m

[<Fact>]
let ``parse vif 0x15 expect volume in m3 with scaler 1e-1`` () =
    [| 0x15uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e-1m

[<Fact>]
let ``parse vif 0x16 expect volume in m3 with scaler 1e0`` () =
    [| 0x16uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e0m

[<Fact>]
let ``parse vif 0x17 expect volume in m3 with scaler 1e1`` () =
    [| 0x17uy |]
    |> parseVib
    |> shouldBeNormal Volume CubicMeters 1e1m

[<Fact>]
let ``parse vif 0x18 expect mass in kg with scaler 1e-3`` () =
    [| 0x18uy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e-3m

[<Fact>]
let ``parse vif 0x19 expect mass in kg with scaler 1e-2`` () =
    [| 0x19uy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e-2m

[<Fact>]
let ``parse vif 0x1A expect mass in kg with scaler 1e-1`` () =
    [| 0x1Auy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e-1m

[<Fact>]
let ``parse vif 0x1B expect mass in kg with scaler 1e0`` () =
    [| 0x1Buy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e0m

[<Fact>]
let ``parse vif 0x1C expect mass in kg with scaler 1e1`` () =
    [| 0x1Cuy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e1m

[<Fact>]
let ``parse vif 0x1D expect mass in kg with scaler 1e2`` () =
    [| 0x1Duy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e2m

[<Fact>]
let ``parse vif 0x1E expect mass kg with scaler 1e3`` () =
    [| 0x1Euy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e3m

[<Fact>]
let ``parse vif 0x1F expect mass in kg with scaler 1e4`` () =
    [| 0x1Fuy |]
    |> parseVib
    |> shouldBeNormal Mass KiloGrams 1e4m

[<Fact>]
let ``parse vif 0x20 expect on-time seconds with scaler 1e0`` () =
    [| 0x20uy |]
    |> parseVib
    |> shouldBeNormal OnTime Seconds 1e0m

[<Fact>]
let ``parse vif 0x21 expect on-time in minutes with scaler 1e0`` () =
    [| 0x21uy |]
    |> parseVib
    |> shouldBeNormal OnTime Minutes 1e0m

[<Fact>]
let ``parse vif 0x22 expect on-time in hours with scaler 1e0`` () =
    [| 0x22uy |]
    |> parseVib
    |> shouldBeNormal OnTime Hours 1e0m

[<Fact>]
let ``parse vif 0x23 expect on-time in days with scaler 1e0`` () =
    [| 0x23uy |]
    |> parseVib
    |> shouldBeNormal OnTime Days 1e0m

[<Fact>]
let ``parse vif 0x24 expect operating time in seconds with scaler 1e0`` () =
    [| 0x24uy |]
    |> parseVib
    |> shouldBeNormal OperatingTime Seconds 1e0m

[<Fact>]
let ``parse vif 0x25 expect operating time in minutes with scaler 1e0`` () =
    [| 0x25uy |]
    |> parseVib
    |> shouldBeNormal OperatingTime Minutes 1e0m

[<Fact>]
let ``parse vif 0x26 expect operating time in hours with scaler 1e0`` () =
    [| 0x26uy |]
    |> parseVib
    |> shouldBeNormal OperatingTime Hours 1e0m

[<Fact>]
let ``parse vif 0x27 expect operating time in days with scaler 1e0`` () =
    [| 0x27uy |]
    |> parseVib
    |> shouldBeNormal OperatingTime Days 1e0m

[<Fact>]
let ``parse vif 0x28 expect power in W with scaler 1e-3`` () =
    [| 0x28uy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e-3m

[<Fact>]
let ``parse vif 0x29 expect power in W with scaler 1e-2`` () =
    [| 0x29uy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e-2m

[<Fact>]
let ``parse vif 0x2A expect power in W with scaler 1e-1`` () =
    [| 0x2Auy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e-1m

[<Fact>]
let ``parse vif 0x2B expect power in W with scaler 1e0`` () =
    [| 0x2Buy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e0m

[<Fact>]
let ``parse vif 0x2C expect power in W with scaler 1e1`` () =
    [| 0x2Cuy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e1m

[<Fact>]
let ``parse vif 0x2D expect power in W with scaler 1e2`` () =
    [| 0x2Duy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e2m

[<Fact>]
let ``parse vif 0x2E expect power in W with scaler 1e3`` () =
    [| 0x2Euy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e3m

[<Fact>]
let ``parse vif 0x2F expect power in W with scaler 1e4`` () =
    [| 0x2Fuy |]
    |> parseVib
    |> shouldBeNormal Power Watts 1e4m

[<Fact>]
let ``parse vif 0x30 expect power in J/h with scaler 1e0`` () =
    [| 0x30uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e0m

[<Fact>]
let ``parse vif 0x31 expect power in J/h with scaler 1e1`` () =
    [| 0x31uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e1m

[<Fact>]
let ``parse vif 0x32 expect power in J/h with scaler 1e2`` () =
    [| 0x32uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e2m

[<Fact>]
let ``parse vif 0x33 expect power in J/h with scaler 1e3`` () =
    [| 0x33uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e3m

[<Fact>]
let ``parse vif 0x34 expect power in J/h with scaler 1e4`` () =
    [| 0x34uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e4m

[<Fact>]
let ``parse vif 0x35 expect power in J/h with scaler 1e5`` () =
    [| 0x35uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e5m

[<Fact>]
let ``parse vif 0x36 expect power in J/h with scaler 1e6`` () =
    [| 0x36uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e6m

[<Fact>]
let ``parse vif 0x37 expect power in J/h with scaler 1e7`` () =
    [| 0x37uy |]
    |> parseVib
    |> shouldBeNormal Power JoulesPerHour 1e7m

[<Fact>]
let ``parse vif 0x38 expect volume flow in m3/h with scaler 1e-6`` () =
    [| 0x38uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-6m

[<Fact>]
let ``parse vif 0x39 expect volume flow in m3/h with scaler 1e-5`` () =
    [| 0x39uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-5m

[<Fact>]
let ``parse vif 0x3A expect volume flow in m3/h with scaler 1e-4`` () =
    [| 0x3Auy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-4m

[<Fact>]
let ``parse vif 0x3B expect volume flow in m3/h with scaler 1e-3`` () =
    [| 0x3Buy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-3m

[<Fact>]
let ``parse vif 0x3C expect volume flow in m3/h with scaler 1e-2`` () =
    [| 0x3Cuy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-2m

[<Fact>]
let ``parse vif 0x3D expect volume flow in m3/h with scaler 1e-1`` () =
    [| 0x3Duy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e-1m

[<Fact>]
let ``parse vif 0x3E expect volume flow in m3/h with scaler 1e0`` () =
    [| 0x3Euy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e0m

[<Fact>]
let ``parse vif 0x3F expect volume flow in m3/h with scaler 1e1`` () =
    [| 0x3Fuy |]
    |> parseVib
    |> shouldBeNormal VolumeFlow CubicMetersPerHour 1e1m

[<Fact>]
let ``parse vif 0x40 expect volume flow ext in m3/min with scaler 1e-7`` () =
    [| 0x40uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-7m

[<Fact>]
let ``parse vif 0x41 expect volume flow ext in m3/min with scaler 1e-6`` () =
    [| 0x41uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-6m

[<Fact>]
let ``parse vif 0x42 expect volume flow ext in m3/min with scaler 1e-5`` () =
    [| 0x42uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-5m

[<Fact>]
let ``parse vif 0x43 expect volume flow ext in m3/min with scaler 1e-4`` () =
    [| 0x43uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-4m

[<Fact>]
let ``parse vif 0x44 expect volume flow ext in m3/min with scaler 1e-3`` () =
    [| 0x44uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-3m

[<Fact>]
let ``parse vif 0x45 expect volume flow ext in m3/min with scaler 1e-2`` () =
    [| 0x45uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-2m

[<Fact>]
let ``parse vif 0x46 expect volume flow ext in m3/min with scaler 1e-1`` () =
    [| 0x46uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e-1m

[<Fact>]
let ``parse vif 0x47 expect volume flow ext in m3/min with scaler 1e0`` () =
    [| 0x47uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerMinute 1e0m

[<Fact>]
let ``parse vif 0x48 expect volume flow ext in m3/s with scaler 1e-9`` () =
    [| 0x48uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-9m

[<Fact>]
let ``parse vif 0x49 expect volume flow ext in m3/s with scaler 1e-8`` () =
    [| 0x49uy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-8m

[<Fact>]
let ``parse vif 0x4A expect volume flow ext in m3/s with scaler 1e-7`` () =
    [| 0x4Auy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-7m

[<Fact>]
let ``parse vif 0x4B expect volume flow ext in m3/s with scaler 1e-6`` () =
    [| 0x4Buy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-6m

[<Fact>]
let ``parse vif 0x4C expect volume flow ext in m3/s with scaler 1e-5`` () =
    [| 0x4Cuy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-5m

[<Fact>]
let ``parse vif 0x4D expect volume flow ext in m3/s with scaler 1e-4`` () =
    [| 0x4Duy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-4m

[<Fact>]
let ``parse vif 0x4E expect volume flow ext in m3/s with scaler 1e-3`` () =
    [| 0x4Euy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-3m

[<Fact>]
let ``parse vif 0x4F expect volume flow ext in m3/s with scaler 1e-2`` () =
    [| 0x4Fuy |]
    |> parseVib
    |> shouldBeNormal VolumeFlowExt CubicMetersPerSecond 1e-2m

[<Fact>]
let ``parse vif 0x50 expect mass flow in kg/h with scaler 1e-3`` () =
    [| 0x50uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e-3m

[<Fact>]
let ``parse vif 0x51 expect mass flow in kg/h with scaler 1e-2`` () =
    [| 0x51uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e-2m

[<Fact>]
let ``parse vif 0x52 expect mass flow in kg/h with scaler 1e-1`` () =
    [| 0x52uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e-1m

[<Fact>]
let ``parse vif 0x53 expect mass flow in kg/h with scaler 1e0`` () =
    [| 0x53uy |]
    |> parseVib

[<Fact>]
let ``parse vif 0x54 expect mass flow in kg/h with scaler 1e1`` () =
    [| 0x54uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e1m

[<Fact>]
let ``parse vif 0x55 expect mass flow in kg/h with scaler 1e2`` () =
    [| 0x55uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e2m

[<Fact>]
let ``parse vif 0x56 expect mass flow in kg/h with scaler 1e3`` () =
    [| 0x56uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e3m

[<Fact>]
let ``parse vif 0x57 expect mass flow in kg/h with scaler 1e4`` () =
    [| 0x57uy |]
    |> parseVib
    |> shouldBeNormal MassFlow KiloGramsPerHour 1e4m

[<Fact>]
let ``parse vif 0x58 expect flow temperature in °C with scaler 1e-3`` () =
    [| 0x58uy |]
    |> parseVib
    |> shouldBeNormal FlowTemperature Celsius 1e-3m

[<Fact>]
let ``parse vif 0x59 expect flow temperature in °C with scaler 1e-2`` () =
    [| 0x59uy |]
    |> parseVib
    |> shouldBeNormal FlowTemperature Celsius 1e-2m

[<Fact>]
let ``parse vif 0x5A expect flow temperature in °C with scaler 1e-1`` () =
    [| 0x5Auy |]
    |> parseVib
    |> shouldBeNormal FlowTemperature Celsius 1e-1m

[<Fact>]
let ``parse vif 0x5B expect flow temperature in °C with scaler 1e0`` () =
    [| 0x5Buy |]
    |> parseVib
    |> shouldBeNormal FlowTemperature Celsius 1e0m

[<Fact>]
let ``parse vif 0x5C expect return temperature in °C with scaler 1e-3`` () =
    [| 0x5Cuy |]
    |> parseVib
    |> shouldBeNormal ReturnTemperature Celsius 1e-3m

[<Fact>]
let ``parse vif 0x5D expect return temperature in °C with scaler 1e-2`` () =
    [| 0x5Duy |]
    |> parseVib
    |> shouldBeNormal ReturnTemperature Celsius 1e-2m

[<Fact>]
let ``parse vif 0x5E expect return temperature in °C with scaler 1e-1`` () =
    [| 0x5Euy |]
    |> parseVib
    |> shouldBeNormal ReturnTemperature Celsius 1e-1m

[<Fact>]
let ``parse vif 0x5F expect return temperature in °C with scaler 1e0`` () =
    [| 0x5Fuy |]
    |> parseVib
    |> shouldBeNormal ReturnTemperature Celsius 1e0m

[<Fact>]
let ``parse vif 0x60 expect temperature difference in K with scaler 1e-3`` () =
    [| 0x60uy |]
    |> parseVib
    |> shouldBeNormal TemperatureDifference Kelvin 1e-3m

[<Fact>]
let ``parse vif 0x61 expect temperature difference in K with scaler 1e-2`` () =
    [| 0x61uy |]
    |> parseVib
    |> shouldBeNormal TemperatureDifference Kelvin 1e-2m

[<Fact>]
let ``parse vif 0x62 expect temperature difference in K with scaler 1e-1`` () =
    [| 0x62uy |]
    |> parseVib
    |> shouldBeNormal TemperatureDifference Kelvin 1e-1m

[<Fact>]
let ``parse vif 0x63 expect temperature difference in K with scaler 1e0`` () =
    [| 0x63uy |]
    |> parseVib
    |> shouldBeNormal TemperatureDifference Kelvin 1e0m

[<Fact>]
let ``parse vif 0x64 expect external temperature in °C with scaler 1e-3`` () =
    [| 0x64uy |]
    |> parseVib
    |> shouldBeNormal ExternalTemperature Celsius 1e-3m

[<Fact>]
let ``parse vif 0x65 expect external temperature in °C with scaler 1e-2`` () =
    [| 0x65uy |]
    |> parseVib
    |> shouldBeNormal ExternalTemperature Celsius 1e-2m

[<Fact>]
let ``parse vif 0x66 expect external temperature in °C with scaler 1e-1`` () =
    [| 0x66uy |]
    |> parseVib
    |> shouldBeNormal ExternalTemperature Celsius 1e-1m

[<Fact>]
let ``parse vif 0x67 expect external temperature in °C with scaler 1e0`` () =
    [| 0x67uy |]
    |> parseVib
    |> shouldBeNormal ExternalTemperature Celsius 1e0m

[<Fact>]
let ``parse vif 0x68 expect pressure in bar with scaler 1e-3`` () =
    [| 0x68uy |]
    |> parseVib
    |> shouldBeNormal Pressure Bar 1e-3m

[<Fact>]
let ``parse vif 0x69 expect pressure in bar with scaler 1e-2`` () =
    [| 0x69uy |]
    |> parseVib
    |> shouldBeNormal Pressure Bar 1e-2m

[<Fact>]
let ``parse vif 0x6A expect pressure in bar with scaler 1e-1`` () =
    [| 0x6Auy |]
    |> parseVib
    |> shouldBeNormal Pressure Bar 1e-1m

[<Fact>]
let ``parse vif 0x6B expect pressure in bar with scaler 1e0`` () =
    [| 0x6Buy |]
    |> parseVib
    |> shouldBeNormal Pressure Bar 1e0m

[<Fact>]
let ``parse vif 0x6C expect date`` () =
    [| 0x6Cuy |]
    |> parseVib
    |> shouldBeNormal Date NoUnit 1e0m

[<Fact>]
let ``parse vif 0x6D expect date and time`` () =
    [| 0x6Duy |]
    |> parseVib
    |> shouldBeNormal DateAndTime NoUnit 1e0m

[<Fact>]
let ``parse vif 0x6E expect units for H.C.A`` () =
    [| 0x6Euy |]
    |> parseVib
    |> shouldBeNormal UnitsForHca NoUnit 1e0m

[<Fact>]
let ``parse vif 0x6F expect invalid vib`` () =
    [| 0x6Fuy |]
    |> parseVib
    |> shouldBeInvalid [| 0x6Fuy |]

[<Fact>]
let ``parse vif 0x70 expect averaging duration in s`` () =
    [| 0x70uy |]
    |> parseVib
    |> shouldBeNormal AveragingDuration Seconds 1e0m

[<Fact>]
let ``parse vif 0x71 expect averaging duration in min`` () =
    [| 0x71uy |]
    |> parseVib
    |> shouldBeNormal AveragingDuration Minutes 1e0m

[<Fact>]
let ``parse vif 0x72 expect averaging duration in h`` () =
    [| 0x72uy |]
    |> parseVib
    |> shouldBeNormal AveragingDuration Hours 1e0m

[<Fact>]
let ``parse vif 0x73 expect averaging duration in d`` () =
    [| 0x73uy |]
    |> parseVib
    |> shouldBeNormal AveragingDuration Days 1e0m

[<Fact>]
let ``parse vif 0x74 expect actuality duration in s`` () =
    [| 0x74uy |]
    |> parseVib
    |> shouldBeNormal ActualityDuration Seconds 1e0m

[<Fact>]
let ``parse vif 0x75 expect actuality duration in min`` () =
    [| 0x75uy |]
    |> parseVib
    |> shouldBeNormal ActualityDuration Minutes 1e0m

[<Fact>]
let ``parse vif 0x76 expect actuality duration in h`` () =
    [| 0x76uy |]
    |> parseVib
    |> shouldBeNormal ActualityDuration Hours 1e0m

[<Fact>]
let ``parse vif 0x77 expect actuality duration in d`` () =
    [| 0x77uy |]
    |> parseVib
    |> shouldBeNormal ActualityDuration Days 1e0m

[<Fact>]
let ``parse vif 0x78 expect fabrication number`` () =
    [| 0x78uy |]
    |> parseVib
    |> shouldBeNormal FabricationNumber NoUnit 1e0m

[<Fact>]
let ``parse vif 0x79 expect enhanced identification`` () =
    [| 0x79uy |]
    |> parseVib
    |> shouldBeNormal Identification NoUnit 1e0m

[<Fact>]
let ``parse vif 0x7A expect address`` () =
    [| 0x7Auy |]
    |> parseVib
    |> shouldBeNormal Address NoUnit 1e0m

[<Fact>]
let ``parse vif 0x7B expect invalid vib`` () =
    [| 0x7Buy |]
    |> parseVib
    |> shouldBeInvalid [| 0x7Buy |]

[<Fact>]
let ``parse vif 0x7C expect parser error`` () =
    [| 0x7Cuy |]
    |> parseVibExpectError
    |> should equal { Pos = 1; Msg = "unexpected end of buffer"; Ctx = [] }

[<Fact>]
let ``parse text vif 0x7C026948 expect text`` () =
    [| 0x7Cuy; 0x02uy; 0x69uy; 0x48uy |]
    |> parseVib
    |> shouldBeText "Hi"

[<Fact>]
let ``parse vif 0x7D expect invalid vib`` () =
    [| 0x7Duy |]
    |> parseVib
    |> shouldBeInvalid [| 0x7Duy |]

[<Fact>]
let ``parse vif 0x7E expect error`` () =
    [| 0x7Euy |]
    |> parseVibExpectError
    |> should equal { Pos = 0; Msg = "Any VIF (0x7E) is not supported"; Ctx = [] }

[<Fact>]
let ``parse vif 0x7F expect mfr specific vib`` () =
    [| 0x7Fuy |]
    |> parseVib
    |> shouldBeMfrSpecific [| 0x7Fuy |]

[<Fact>]
let ``parse vif 0x80 expect error`` () =
    [| 0x80uy |]
    |> parseVibExpectError
    |> should equal { Pos = 1; Msg = "unexpected end of buffer"; Ctx = [] }

[<Fact>]
let ``parse vib 0x8000 expect extension NoError`` () =
    [| 0x80uy; 0x00uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x00uy ]

[<Fact>]
let ``parse vib 0x8001 expect extension TooManyDIFEs`` () =
    [| 0x80uy; 0x01uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x01uy ]

[<Fact>]
let ``parse vib 0x8002 expect extension StorageNumberNotImplemented`` () =
    [| 0x80uy; 0x02uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x02uy ]

[<Fact>]
let ``parse vib 0x8003 expect extension UnitNumberNotImplemented`` () =
    [| 0x80uy; 0x03uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x03uy ]

[<Fact>]
let ``parse vib 0x8004 expect extension TariffNumberNotImplemented`` () =
    [| 0x80uy; 0x04uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x04uy ]

[<Fact>]
let ``parse vib 0x8005 expect extension FunctionNotImplemented`` () =
    [| 0x80uy; 0x05uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x05uy ]

[<Fact>]
let ``parse vib 0x8006 expect extension DataClassNotImplemented`` () =
    [| 0x80uy; 0x06uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x06uy ]

[<Fact>]
let ``parse vib 0x8007 expect extension DataSizeNotImplemented`` () =
    [| 0x80uy; 0x07uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x07uy ]

[<Fact>]
let ``parse vib 0x8008 expect invalid extension`` () =
    [| 0x80uy; 0x08uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x08uy ]

[<Fact>]
let ``parse vib 0x8009 expect invalid extension`` () =
    [| 0x80uy; 0x09uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x09uy ]

[<Fact>]
let ``parse vib 0x800A expect invalid extension`` () =
    [| 0x80uy; 0x0Auy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Auy ]

[<Fact>]
let ``parse vib 0x800B expect extension TooManyVIFEs`` () =
    [| 0x80uy; 0x0Buy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Buy ]

[<Fact>]
let ``parse vib 0x800C expect extension IllegalVifGroup`` () =
    [| 0x80uy; 0x0Cuy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Cuy ]

[<Fact>]
let ``parse vib 0x800D expect extension IllegalVifExponent`` () =
    [| 0x80uy; 0x0Duy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Duy ]

[<Fact>]
let ``parse vib 0x800E expect extension VifDifMismatch`` () =
    [| 0x80uy; 0x0Euy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Euy ]

[<Fact>]
let ``parse vib 0x800F expect extension UnimplementedAction`` () =
    [| 0x80uy; 0x0Fuy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x0Fuy ]

[<Fact>]
let ``parse vib 0x8010 expect invalid extension`` () =
    [| 0x80uy; 0x10uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x10uy ]

[<Fact>]
let ``parse vib 0x8011 expect invalid extension`` () =
    [| 0x80uy; 0x11uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x11uy ]

[<Fact>]
let ``parse vib 0x8012 expect extension AverageValue`` () =
    [| 0x80uy; 0x12uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.AverageValue ]

[<Fact>]
let ``parse vib 0x8013 expect extension InverseCompactProfile`` () =
    [| 0x80uy; 0x13uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.InverseCompactProfile ]

[<Fact>]
let ``parse vib 0x8014 expect extension RelativeDeviation`` () =
    [| 0x80uy; 0x14uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.RelativeDeviation ]

[<Fact>]
let ``parse vib 0x8015 expect extension NoDataAvailable`` () =
    [| 0x80uy; 0x15uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x15uy ]

[<Fact>]
let ``parse vib 0x8016 expect extension DataOverflow`` () =
    [| 0x80uy; 0x16uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x16uy ]

[<Fact>]
let ``parse vib 0x8017 expect extension DataUnderflow`` () =
    [| 0x80uy; 0x17uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x17uy ]

[<Fact>]
let ``parse vib 0x8018 expect extension DataError`` () =
    [| 0x80uy; 0x18uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x18uy ]

[<Fact>]
let ``parse vib 0x8019 expect invalid extension`` () =
    [| 0x80uy; 0x19uy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x19uy ]

[<Fact>]
let ``parse vib 0x801A expect invalid extension`` () =
    [| 0x80uy; 0x1Auy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x1Auy ]

[<Fact>]
let ``parse vib 0x801B expect invalid extension`` () =
    [| 0x80uy; 0x1Buy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x1Buy ]

[<Fact>]
let ``parse vib 0x801C expect extension PrematureEndOfRecord`` () =
    [| 0x80uy; 0x1Cuy |]
    |> parseVib
    |> shouldHaveExtensions [ InvalidCombVifExt 0x1Cuy ]

[<Fact>]
let ``parse vib 0x801D expect extension StandardConformDataContent`` () =
    [| 0x80uy; 0x1Duy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.StandardConformDataContent ]

[<Fact>]
let ``parse vib 0x801E expect extension CompactProfileWithRegisterNumbers`` () =
    [| 0x80uy; 0x1Euy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.CompactProfileWithRegisterNumbers ]

[<Fact>]
let ``parse vib 0x801F expect extension CompactProfile`` () =
    [| 0x80uy; 0x1Fuy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.CompactProfile ]

[<Fact>]
let ``parse vib 0x8020 expect extension PerSecond`` () =
    [| 0x80uy; 0x20uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerSecond ]

[<Fact>]
let ``parse vib 0x8021 expect extension PerMinute`` () =
    [| 0x80uy; 0x21uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerMinute ]

[<Fact>]
let ``parse vib 0x8022 expect extension PerHour`` () =
    [| 0x80uy; 0x22uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerHour ]

[<Fact>]
let ``parse vib 0x8023 expect extension PerDay`` () =
    [| 0x80uy; 0x23uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerDay ]

[<Fact>]
let ``parse vib 0x8024 expect extension PerWeek`` () =
    [| 0x80uy; 0x24uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerWeek ]

[<Fact>]
let ``parse vib 0x8025 expect extension PerMonth`` () =
    [| 0x80uy; 0x25uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerMonth ]

[<Fact>]
let ``parse vib 0x8026 expect extension PerYear`` () =
    [| 0x80uy; 0x26uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerYear ]

[<Fact>]
let ``parse vib 0x8027 expect extension PerRevolutionMeasurement`` () =
    [| 0x80uy; 0x27uy |]
    |> parseVib
    |> shouldHaveExtensions [ ValidCombVifExt MbusValueTypeExtension.PerRevolutionMeasurement ]