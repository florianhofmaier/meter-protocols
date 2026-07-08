namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus
open Metering.Mbus.Protocol.Records

type Vif =
    {
        Val: MbusValueType
        Unit: MbusUnit
        Scaler: decimal
    }

module Vif =

    let private primTable =
        Map [
            0x00uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e-3m }
            0x01uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e-2m }
            0x02uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e-1m }
            0x03uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e0m }
            0x04uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e1m }
            0x05uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e2m }
            0x06uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e3m }
            0x07uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e4m }
            0x08uy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e0m }
            0x09uy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e1m }
            0x0Auy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e2m }
            0x0Buy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e3m }
            0x0Cuy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e4m }
            0x0Duy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e5m }
            0x0Euy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e6m }
            0x0Fuy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e7m }
            0x10uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-6m }
            0x11uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-5m }
            0x12uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-4m }
            0x13uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-3m }
            0x14uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-2m }
            0x15uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-1m }
            0x16uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e0m }
            0x17uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e1m }
            0x18uy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e-3m }
            0x19uy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e-2m }
            0x1Auy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e-1m }
            0x1Buy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e0m }
            0x1Cuy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e1m }
            0x1Duy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e2m }
            0x1Euy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e3m }
            0x1Fuy, { Val = MbusValueType.Mass; Unit = MbusUnit.KiloGrams; Scaler = 1e4m }
            0x20uy, { Val = MbusValueType.OnTime; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x21uy, { Val = MbusValueType.OnTime; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x22uy, { Val = MbusValueType.OnTime; Unit = MbusUnit.Hours; Scaler = 1m }
            0x23uy, { Val = MbusValueType.OnTime; Unit = MbusUnit.Days; Scaler = 1m }
            0x24uy, { Val = MbusValueType.OperatingTime; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x25uy, { Val = MbusValueType.OperatingTime; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x26uy, { Val = MbusValueType.OperatingTime; Unit = MbusUnit.Hours; Scaler = 1m }
            0x27uy, { Val = MbusValueType.OperatingTime; Unit = MbusUnit.Days; Scaler = 1m }
            0x28uy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e-3m }
            0x29uy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e-2m }
            0x2Auy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e-1m }
            0x2Buy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e0m }
            0x2Cuy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e1m }
            0x2Duy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e2m }
            0x2Euy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e3m }
            0x2Fuy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e4m }
            0x30uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e0m }
            0x31uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e1m }
            0x32uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e2m }
            0x33uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e3m }
            0x34uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e4m }
            0x35uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e5m }
            0x36uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e6m }
            0x37uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e7m }
            0x38uy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-6m }
            0x39uy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-5m }
            0x3Auy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-4m }
            0x3Buy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-3m }
            0x3Cuy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-2m }
            0x3Duy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e-1m }
            0x3Euy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e0m }
            0x3Fuy, { Val = MbusValueType.VolumeFlow; Unit = MbusUnit.CubicMetersPerHour; Scaler = 1e1m }
            0x40uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-7m }
            0x41uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-6m }
            0x42uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-5m }
            0x43uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-4m }
            0x44uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-3m }
            0x45uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-2m }
            0x46uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e-1m }
            0x47uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerMinute; Scaler = 1e0m }
            0x48uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-9m }
            0x49uy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-8m }
            0x4Auy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-7m }
            0x4Buy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-6m }
            0x4Cuy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-5m }
            0x4Duy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-4m }
            0x4Euy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-3m }
            0x4Fuy, { Val = MbusValueType.VolumeFlowExt; Unit = MbusUnit.CubicMetersPerSecond; Scaler = 1e-2m }
            0x50uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e-3m }
            0x51uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e-2m }
            0x52uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e-1m }
            0x53uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e0m }
            0x54uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e1m }
            0x55uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e2m }
            0x56uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e3m }
            0x57uy, { Val = MbusValueType.MassFlow; Unit = MbusUnit.KiloGramsPerHour; Scaler = 1e4m }
            0x58uy, { Val = MbusValueType.FlowTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-3m }
            0x59uy, { Val = MbusValueType.FlowTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-2m }
            0x5Auy, { Val = MbusValueType.FlowTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-1m }
            0x5Buy, { Val = MbusValueType.FlowTemperature; Unit = MbusUnit.Celsius; Scaler = 1e0m }
            0x5Cuy, { Val = MbusValueType.ReturnTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-3m }
            0x5Duy, { Val = MbusValueType.ReturnTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-2m }
            0x5Euy, { Val = MbusValueType.ReturnTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-1m }
            0x5Fuy, { Val = MbusValueType.ReturnTemperature; Unit = MbusUnit.Celsius; Scaler = 1e0m }
            0x60uy, { Val = MbusValueType.TemperatureDifference; Unit = MbusUnit.Kelvin; Scaler = 1e-3m }
            0x61uy, { Val = MbusValueType.TemperatureDifference; Unit = MbusUnit.Kelvin; Scaler = 1e-2m }
            0x62uy, { Val = MbusValueType.TemperatureDifference; Unit = MbusUnit.Kelvin; Scaler = 1e-1m }
            0x63uy, { Val = MbusValueType.TemperatureDifference; Unit = MbusUnit.Kelvin; Scaler = 1e0m }
            0x64uy, { Val = MbusValueType.ExternalTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-3m }
            0x65uy, { Val = MbusValueType.ExternalTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-2m }
            0x66uy, { Val = MbusValueType.ExternalTemperature; Unit = MbusUnit.Celsius; Scaler = 1e-1m }
            0x67uy, { Val = MbusValueType.ExternalTemperature; Unit = MbusUnit.Celsius; Scaler = 1e0m }
            0x68uy, { Val = MbusValueType.Pressure; Unit = MbusUnit.Bar; Scaler = 1e-3m }
            0x69uy, { Val = MbusValueType.Pressure; Unit = MbusUnit.Bar; Scaler = 1e-2m }
            0x6Auy, { Val = MbusValueType.Pressure; Unit = MbusUnit.Bar; Scaler = 1e-1m }
            0x6Buy, { Val = MbusValueType.Pressure; Unit = MbusUnit.Bar; Scaler = 1e0m }
            0x6Cuy, { Val = MbusValueType.Date; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x6Duy, { Val = MbusValueType.DateAndTime; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x6Euy, { Val = MbusValueType.UnitsForHca; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x70uy, { Val = MbusValueType.AveragingDuration; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x71uy, { Val = MbusValueType.AveragingDuration; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x72uy, { Val = MbusValueType.AveragingDuration; Unit = MbusUnit.Hours; Scaler = 1m }
            0x73uy, { Val = MbusValueType.AveragingDuration; Unit = MbusUnit.Days; Scaler = 1m }
            0x74uy, { Val = MbusValueType.ActualityDuration; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x75uy, { Val = MbusValueType.ActualityDuration; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x76uy, { Val = MbusValueType.ActualityDuration; Unit = MbusUnit.Hours; Scaler = 1m }
            0x77uy, { Val = MbusValueType.ActualityDuration; Unit = MbusUnit.Days; Scaler = 1m }
            0x78uy, { Val = MbusValueType.FabricationNumber; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x79uy, { Val = MbusValueType.Identification; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x7Auy, { Val = MbusValueType.Address; Unit = MbusUnit.NoUnit; Scaler = 1m }
        ]

    let private altExtTable =
        Map [
            0x00uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e5m }
            0x01uy, { Val = MbusValueType.Energy; Unit = MbusUnit.WattHours; Scaler = 1e6m }
            0x02uy, { Val = MbusValueType.ReactiveEnergy; Unit = MbusUnit.VoltAmpereReactiveHours; Scaler = 1e3m }
            0x03uy, { Val = MbusValueType.ReactiveEnergy; Unit = MbusUnit.VoltAmpereReactiveHours; Scaler = 1e4m }
            0x04uy, { Val = MbusValueType.ApparentEnergy; Unit = MbusUnit.VoltAmpereHours; Scaler = 1e3m }
            0x05uy, { Val = MbusValueType.ApparentEnergy; Unit = MbusUnit.VoltAmpereHours; Scaler = 1e4m }
            0x08uy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e8m }
            0x09uy, { Val = MbusValueType.Energy; Unit = MbusUnit.Joules; Scaler = 1e9m }
            0x0Cuy, { Val = MbusValueType.Energy; Unit = MbusUnit.Calories; Scaler = 1e5m }
            0x0Duy, { Val = MbusValueType.Energy; Unit = MbusUnit.Calories; Scaler = 1e6m }
            0x0Euy, { Val = MbusValueType.Energy; Unit = MbusUnit.Calories; Scaler = 1e7m }
            0x0Fuy, { Val = MbusValueType.Energy; Unit = MbusUnit.Calories; Scaler = 1e8m }
            0x10uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e2m }
            0x11uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e3m }
            0x14uy, { Val = MbusValueType.ReactivePower; Unit = MbusUnit.VoltAmpereReactive; Scaler = 1e0m }
            0x15uy, { Val = MbusValueType.ReactivePower; Unit = MbusUnit.VoltAmpereReactive; Scaler = 1e1m }
            0x16uy, { Val = MbusValueType.ReactivePower; Unit = MbusUnit.VoltAmpereReactive; Scaler = 1e2m }
            0x17uy, { Val = MbusValueType.ReactivePower; Unit = MbusUnit.VoltAmpereReactive; Scaler = 1e3m }
            0x18uy, { Val = MbusValueType.Mass; Unit = MbusUnit.Tons; Scaler = 1e2m }
            0x19uy, { Val = MbusValueType.Mass; Unit = MbusUnit.Tons; Scaler = 1e3m }
            0x1Auy, { Val = MbusValueType.RelativeHumidity; Unit = MbusUnit.Percent; Scaler = 1e-1m }
            0x1Buy, { Val = MbusValueType.RelativeHumidity; Unit = MbusUnit.Percent; Scaler = 1e0m }
            0x20uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicFeet; Scaler = 1e0m }
            0x21uy, { Val = MbusValueType.Volume; Unit = MbusUnit.CubicFeet; Scaler = 1e-1m }
            0x28uy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e5m }
            0x29uy, { Val = MbusValueType.Power; Unit = MbusUnit.Watts; Scaler = 1e6m }
            0x2Auy, { Val = MbusValueType.PhaseUToU; Unit = MbusUnit.Degrees; Scaler = 1e-1m }
            0x2Buy, { Val = MbusValueType.PhaseUToI; Unit = MbusUnit.Degrees; Scaler = 1e-1m }
            0x2Cuy, { Val = MbusValueType.Frequency; Unit = MbusUnit.Hertz; Scaler = 1e-3m }
            0x2Duy, { Val = MbusValueType.Frequency; Unit = MbusUnit.Hertz; Scaler = 1e-2m }
            0x2Euy, { Val = MbusValueType.Frequency; Unit = MbusUnit.Hertz; Scaler = 1e-1m }
            0x2Fuy, { Val = MbusValueType.Frequency; Unit = MbusUnit.Hertz; Scaler = 1e0m }
            0x30uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e8m }
            0x31uy, { Val = MbusValueType.Power; Unit = MbusUnit.JoulesPerHour; Scaler = 1e9m }
            0x34uy, { Val = MbusValueType.ApparentPower; Unit = MbusUnit.VoltAmpere; Scaler = 1e0m }
            0x35uy, { Val = MbusValueType.ApparentPower; Unit = MbusUnit.VoltAmpere; Scaler = 1e1m }
            0x36uy, { Val = MbusValueType.ApparentPower; Unit = MbusUnit.VoltAmpere; Scaler = 1e2m }
            0x37uy, { Val = MbusValueType.ApparentPower; Unit = MbusUnit.VoltAmpere; Scaler = 1e3m }
            0x74uy, { Val = MbusValueType.ColdWarmTemperatureLimit; Unit = MbusUnit.Celsius; Scaler = 1e-3m }
            0x75uy, { Val = MbusValueType.ColdWarmTemperatureLimit; Unit = MbusUnit.Celsius; Scaler = 1e-2m }
            0x76uy, { Val = MbusValueType.ColdWarmTemperatureLimit; Unit = MbusUnit.Celsius; Scaler = 1e-1m }
            0x77uy, { Val = MbusValueType.ColdWarmTemperatureLimit; Unit = MbusUnit.Celsius; Scaler = 1e0m }
            0x78uy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e-3m }
            0x79uy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e-2m }
            0x7Auy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e-1m }
            0x7Buy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e0m }
            0x7Cuy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e1m }
            0x7Duy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e2m }
            0x7Euy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e3m }
            0x7Fuy, { Val = MbusValueType.CumulativeMaximumOfActivePower; Unit = MbusUnit.Watts; Scaler = 1e4m }
        ]

    let private mainExtTable =
        Map [
            0x00uy, { Val = MbusValueType.Credit; Unit = MbusUnit.NoUnit; Scaler = 1e-3m }
            0x01uy, { Val = MbusValueType.Credit; Unit = MbusUnit.NoUnit; Scaler = 1e-2m }
            0x02uy, { Val = MbusValueType.Credit; Unit = MbusUnit.NoUnit; Scaler = 1e-1m }
            0x03uy, { Val = MbusValueType.Credit; Unit = MbusUnit.NoUnit; Scaler = 1e0m }
            0x04uy, { Val = MbusValueType.Debit; Unit = MbusUnit.NoUnit; Scaler = 1e-3m }
            0x05uy, { Val = MbusValueType.Debit; Unit = MbusUnit.NoUnit; Scaler = 1e-2m }
            0x06uy, { Val = MbusValueType.Debit; Unit = MbusUnit.NoUnit; Scaler = 1e-1m }
            0x07uy, { Val = MbusValueType.Debit; Unit = MbusUnit.NoUnit; Scaler = 1e0m }
            0x08uy, { Val = MbusValueType.UniqueMessageIdentification; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x09uy, { Val = MbusValueType.DeviceType; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Auy, { Val = MbusValueType.Manufacturer; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Buy, { Val = MbusValueType.ParameterSetIdentification; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Cuy, { Val = MbusValueType.Model; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Duy, { Val = MbusValueType.HardwareVersionNumber; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Euy, { Val = MbusValueType.MetrologyVersionNumber; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x0Fuy, { Val = MbusValueType.OtherSoftwareVersionNumber; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x10uy, { Val = MbusValueType.CustomerLocation; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x11uy, { Val = MbusValueType.Customer; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x12uy, { Val = MbusValueType.AccessCodeUser; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x13uy, { Val = MbusValueType.AccessCodeOperator; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x14uy, { Val = MbusValueType.AccessCodeSystemOperator; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x15uy, { Val = MbusValueType.AccessCodeDeveloper; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x16uy, { Val = MbusValueType.Password; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x17uy, { Val = MbusValueType.ErrorFlags; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x18uy, { Val = MbusValueType.ErrorMask; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x19uy, { Val = MbusValueType.SecurityKey; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x1Auy, { Val = MbusValueType.DigitalOutput; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x1Buy, { Val = MbusValueType.DigitalInput; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x1Cuy, { Val = MbusValueType.BaudRate; Unit = MbusUnit.KiloBitsPerSecond; Scaler = 1m }
            0x1Duy, { Val = MbusValueType.ResponseDelayTime; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x1Euy, { Val = MbusValueType.Retry; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x1Fuy, { Val = MbusValueType.RemoteControl; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x20uy, { Val = MbusValueType.FirstStorageNumberForCyclicStorage; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x21uy, { Val = MbusValueType.LastStorageNumberForCyclicStorage; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x22uy, { Val = MbusValueType.SizeOfStorageBlock; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x24uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x25uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x26uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Hours; Scaler = 1m }
            0x27uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Days; Scaler = 1m }
            0x28uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Months; Scaler = 1m }
            0x29uy, { Val = MbusValueType.StorageInterval; Unit = MbusUnit.Years; Scaler = 1m }
            0x2Auy, { Val = MbusValueType.OperatorSpecificData; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x2Buy, { Val = MbusValueType.TimePointSecond; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x2Cuy, { Val = MbusValueType.DurationSinceLastReadout; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x2Duy, { Val = MbusValueType.DurationSinceLastReadout; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x2Euy, { Val = MbusValueType.DurationSinceLastReadout; Unit = MbusUnit.Hours; Scaler = 1m }
            0x2Fuy, { Val = MbusValueType.DurationSinceLastReadout; Unit = MbusUnit.Days; Scaler = 1m }
            0x30uy, { Val = MbusValueType.StartOfTariff; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x31uy, { Val = MbusValueType.DurationOfTariff; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x32uy, { Val = MbusValueType.DurationOfTariff; Unit = MbusUnit.Hours; Scaler = 1m }
            0x33uy, { Val = MbusValueType.DurationOfTariff; Unit = MbusUnit.Days; Scaler = 1m }
            0x34uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x35uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x36uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Hours; Scaler = 1m }
            0x37uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Days; Scaler = 1m }
            0x38uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Months; Scaler = 1m }
            0x39uy, { Val = MbusValueType.PeriodOfTariff; Unit = MbusUnit.Years; Scaler = 1m }
            0x3Auy, { Val = MbusValueType.NoVif; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x3Buy, { Val = MbusValueType.DataContainerForWMbusProtocol; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x3Cuy, { Val = MbusValueType.PeriodOfNominalDataTransmissions; Unit = MbusUnit.Seconds; Scaler = 1m }
            0x3Duy, { Val = MbusValueType.PeriodOfNominalDataTransmissions; Unit = MbusUnit.Minutes; Scaler = 1m }
            0x3Euy, { Val = MbusValueType.PeriodOfNominalDataTransmissions; Unit = MbusUnit.Hours; Scaler = 1m }
            0x3Fuy, { Val = MbusValueType.PeriodOfNominalDataTransmissions; Unit = MbusUnit.Days; Scaler = 1m }
            0x40uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-9m }
            0x41uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-8m }
            0x42uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-7m }
            0x43uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-6m }
            0x44uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-5m }
            0x45uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-4m }
            0x46uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-3m }
            0x47uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-2m }
            0x48uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e-1m }
            0x49uy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e0m }
            0x4Auy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e1m }
            0x4Buy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e2m }
            0x4Cuy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e3m }
            0x4Duy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e4m }
            0x4Euy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e5m }
            0x4Fuy, { Val = MbusValueType.Voltage; Unit = MbusUnit.Volts; Scaler = 1e6m }
            0x50uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-12m }
            0x51uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-11m }
            0x52uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-10m }
            0x53uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-9m }
            0x54uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-8m }
            0x55uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-7m }
            0x56uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-6m }
            0x57uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-5m }
            0x58uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-4m }
            0x59uy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-3m }
            0x5Auy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-2m }
            0x5Buy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e-1m }
            0x5Cuy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e0m }
            0x5Duy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e1m }
            0x5Euy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e2m }
            0x5Fuy, { Val = MbusValueType.Current; Unit = MbusUnit.Amps; Scaler = 1e3m }
            0x60uy, { Val = MbusValueType.ResetCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x61uy, { Val = MbusValueType.AccumulationCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x62uy, { Val = MbusValueType.ControlSignal; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x63uy, { Val = MbusValueType.DayOfWeek; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x64uy, { Val = MbusValueType.WeekNumber; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x65uy, { Val = MbusValueType.TimePointOfDayChange; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x66uy, { Val = MbusValueType.StateOfParameterActivation; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x67uy, { Val = MbusValueType.SpecialSupplierInformation; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x68uy, { Val = MbusValueType.DurationSinceLastAccumulation; Unit = MbusUnit.Hours; Scaler = 1m }
            0x69uy, { Val = MbusValueType.DurationSinceLastAccumulation; Unit = MbusUnit.Days; Scaler = 1m }
            0x6Auy, { Val = MbusValueType.DurationSinceLastAccumulation; Unit = MbusUnit.Months; Scaler = 1m }
            0x6Buy, { Val = MbusValueType.DurationSinceLastAccumulation; Unit = MbusUnit.Years; Scaler = 1m }
            0x6Cuy, { Val = MbusValueType.OperatingTimeBattery; Unit = MbusUnit.Hours; Scaler = 1m }
            0x6Duy, { Val = MbusValueType.OperatingTimeBattery; Unit = MbusUnit.Days; Scaler = 1m }
            0x6Euy, { Val = MbusValueType.OperatingTimeBattery; Unit = MbusUnit.Months; Scaler = 1m }
            0x6Fuy, { Val = MbusValueType.OperatingTimeBattery; Unit = MbusUnit.Years; Scaler = 1m }
            0x70uy, { Val = MbusValueType.DateAndTimeOfBatteryChange; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x71uy, { Val = MbusValueType.RfLevel; Unit = MbusUnit.DecibelMilliWatts; Scaler = 1m }
            0x72uy, { Val = MbusValueType.DaylightSavings; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x73uy, { Val = MbusValueType.ListeningWindowManagement; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x74uy, { Val = MbusValueType.RemainingBatteryLifetime; Unit = MbusUnit.Days; Scaler = 1m }
            0x75uy, { Val = MbusValueType.NumberOfTimesTheMeterWasStopped; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x76uy, { Val = MbusValueType.DataContainerForManufacturerSpecificProtocol; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x77uy, { Val = MbusValueType.DataContainerForMbusUpperLayers; Unit = MbusUnit.NoUnit; Scaler = 1m }
    ]

    let private mainExtTableSndLevel =
        Map [
            0x00uy, { Val = MbusValueType.CurrentSelectedApplication; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x01uy, { Val = MbusValueType.SubDeviceType; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x02uy, { Val = MbusValueType.RemainingBatteryLifetime; Unit = MbusUnit.Months; Scaler = 1m }
            0x03uy, { Val = MbusValueType.RemainingBatteryLifetime; Unit = MbusUnit.Years; Scaler = 1m }
            0x04uy, { Val = MbusValueType.NumberOfAvailableCommunicationCreditsOnTheLocalInterface; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x05uy, { Val = MbusValueType.NumberOfAvailableCommunicationCreditsOnTheWirelessMbusInterface; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x08uy, { Val = MbusValueType.InstallationConditions; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x10uy, { Val = MbusValueType.Co2Content; Unit = MbusUnit.Ppm; Scaler = 1e0m }
            0x11uy, { Val = MbusValueType.Co2Content; Unit = MbusUnit.Ppm; Scaler = 1e1m }
            0x12uy, { Val = MbusValueType.CoContent; Unit = MbusUnit.Ppm; Scaler = 1e0m }
            0x13uy, { Val = MbusValueType.CoContent; Unit = MbusUnit.Ppm; Scaler = 1e1m }
            0x14uy, { Val = MbusValueType.VocContent; Unit = MbusUnit.Ppb; Scaler = 1e0m }
            0x15uy, { Val = MbusValueType.VocContent; Unit = MbusUnit.Ppb; Scaler = 1e1m }
            0x16uy, { Val = MbusValueType.VocContent; Unit = MbusUnit.MicroGramsPerCubicMeter; Scaler = 1e0m }
            0x17uy, { Val = MbusValueType.ParticlesUnspecifiedRange; Unit = MbusUnit.MicroGramsPerCubicMeter; Scaler = 1e0m }
            0x18uy, { Val = MbusValueType.ParticlesPm1; Unit = MbusUnit.MicroGramsPerCubicMeter; Scaler = 1e0m }
            0x19uy, { Val = MbusValueType.ParticlesPm2_5; Unit = MbusUnit.MicroGramsPerCubicMeter; Scaler = 1e0m }
            0x1Auy, { Val = MbusValueType.ParticlesPm10; Unit = MbusUnit.MicroGramsPerCubicMeter; Scaler = 1e0m }
            0x1Buy, { Val = MbusValueType.ParticlesUnspecifiedRange; Unit = MbusUnit.MillionUnitsPerCubicMeter; Scaler = 1e0m }
            0x1Cuy, { Val = MbusValueType.ParticlesPm1; Unit = MbusUnit.MillionUnitsPerCubicMeter; Scaler = 1e0m }
            0x1Duy, { Val = MbusValueType.ParticlesPm2_5; Unit = MbusUnit.MillionUnitsPerCubicMeter; Scaler = 1e0m }
            0x1Euy, { Val = MbusValueType.ParticlesPm10; Unit = MbusUnit.MillionUnitsPerCubicMeter; Scaler = 1e0m }
            0x1Fuy, { Val = MbusValueType.Illuminance; Unit = MbusUnit.Lux; Scaler = 1e0m }
            0x20uy, { Val = MbusValueType.LuminousIntensity; Unit = MbusUnit.Candela; Scaler = 1e0m }
            0x21uy, { Val = MbusValueType.Irradiance; Unit = MbusUnit.WattsPerSquareMeter; Scaler = 1e0m }
            0x22uy, { Val = MbusValueType.WindSpeed; Unit = MbusUnit.KilometersPerHour; Scaler = 1e0m }
            0x23uy, { Val = MbusValueType.Rainfall; Unit = MbusUnit.LiterPerSquareMillimeter; Scaler = 1e0m }
            0x24uy, { Val = MbusValueType.Noise; Unit = MbusUnit.DecibelAWeighted; Scaler = 1e0m }
            0x25uy, { Val = MbusValueType.Turbidity; Unit = MbusUnit.FormazinNephelometricUnit; Scaler = 1e0m }
            0x27uy, { Val = MbusValueType.PhValue; Unit = MbusUnit.NoUnit; Scaler = 1e-1m }
            0x2Cuy, { Val = MbusValueType.NumberOfDismounts; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x2Duy, { Val = MbusValueType.NumberOfTestButtonOperatedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x2Euy, { Val = MbusValueType.NumberOfAlarms; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x2Fuy, { Val = MbusValueType.NumberOfAlarmMuteSwitchOperatedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x30uy, { Val = MbusValueType.NumberOfObstacleDetectedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x31uy, { Val = MbusValueType.SmokeEntriesBlockingCumulatedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x32uy, { Val = MbusValueType.SmokeChamberDefectCumulatedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x33uy, { Val = MbusValueType.NumberOfSelfTestFailedCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x34uy, { Val = MbusValueType.NumberOfSounderDefectCounter; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x35uy, { Val = MbusValueType.NumberOfCoAlarmsLowLevel; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x36uy, { Val = MbusValueType.NumberOfCoAlarmsMediumLevel; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x37uy, { Val = MbusValueType.NumberOfCoAlarmsHighLevel; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x38uy, { Val = MbusValueType.BatteryStatus; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x39uy, { Val = MbusValueType.ChamberPollutionLevel; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x3Auy, { Val = MbusValueType.Distance; Unit = MbusUnit.Millimeter; Scaler = 1e0m }
            0x3Buy, { Val = MbusValueType.Distance; Unit = MbusUnit.Millimeter; Scaler = 1e1m }
            0x3Euy, { Val = MbusValueType.MoistureLevel; Unit = MbusUnit.Percent; Scaler = 1e0m }
            0x3Fuy, { Val = MbusValueType.TypeOrClassOfApproval; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x40uy, { Val = MbusValueType.StatusBitsForPressureDevices; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x41uy, { Val = MbusValueType.StatusBitsForSmokeAlarmDevices; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x42uy, { Val = MbusValueType.StatusBitsForCoAlarmDevices; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x43uy, { Val = MbusValueType.StatusBitsForHeatAlarmDevices; Unit = MbusUnit.NoUnit; Scaler = 1m }
            0x44uy, { Val = MbusValueType.StatusBitsForDoorContactSensorAndLockedDoorDetector; Unit = MbusUnit.NoUnit; Scaler = 1m }
        ]

    let private mainExt = 0xFDuy
    let private altExt = 0xFBuy

    let private tryMapPrim =
        Utility.tryMap primTable

    let private tryMapMainExtFirstLevel =
        Utility.tryMap mainExtTable

    let private tryMapMainExtSecondLevel =
        Utility.tryMap mainExtTableSndLevel

    let private tryMapMainExt (bytes: ReadOnlyMemory<byte>) pos =
        if pos < bytes.Length && bytes.Span[pos] = mainExt then
            tryMapMainExtSecondLevel bytes (pos + 1)
        else
            tryMapMainExtFirstLevel bytes pos

    let private tryMapAltExt =
        Utility.tryMap altExtTable

    let private tryMapVif (bytes: ReadOnlyMemory<byte>) pos =
        if pos >= bytes.Length then
            None, pos
        else
            match bytes.Span[pos] with
            | code when code = mainExt -> tryMapMainExt bytes (pos + 1)
            | code when code = altExt -> tryMapAltExt bytes (pos + 1)
            | _ -> tryMapPrim bytes pos

    let map
        (infoBlock: ParsedField<InfoBlockRaw>)
        (pos: int)
        : Validation<Vif> * int =

        let bytes = InfoBlockRaw.bytes infoBlock.Value
        let vif, pos = tryMapVif bytes pos

        match vif with
        | Some v -> passed v, pos
        | None when pos < bytes.Length ->
            failed infoBlock $"Unknown VIF code 0x{bytes.Span[pos]:X2} at index {pos}", pos
        | None ->
            failed infoBlock $"Missing VIF code at index {pos}", pos
