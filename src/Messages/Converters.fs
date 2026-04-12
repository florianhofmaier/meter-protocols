namespace Mbus.Messages.Converters

module MbusValue =

    open System
    open Mbus.Records
    open Mbus.Records.ValueInfoBlocks

    let getScaler vib =
        match vib with
        | RspVib.Normal nv -> nv.Def.Scaler
        | _ -> 1.0m

    let padTo8Bytes (bytes: ReadOnlyMemory<byte>): byte[] =
        let arr = bytes.ToArray()
        let padded = Array.zeroCreate<byte> 8
        Array.blit arr 0 padded 0 arr.Length
        padded

    let getNumericValue (r:RspDataRecord) : double =
        let scale v = getScaler r.Vib * v |> double
        match r.Value with
        | Bcd2Digit v -> v |> decimal |> scale
        | Bcd4Digit v -> v |> decimal |> scale
        | Bcd6Digit v -> v |> decimal |> scale
        | Bcd8Digit v -> v |> decimal |> scale
        | Bcd12Digit v -> v |> decimal |> scale
        | Real32 v -> v |> decimal |> scale
        | Int8 v -> v |> decimal |> scale
        | Int16 v -> v |> decimal |> scale
        | Int24 v -> v |> decimal |> scale
        | Int32 v -> v |> decimal |> scale
        | Int48 v -> v |> decimal |> scale
        | Int64 v -> v |> decimal |> scale
        | NoData -> 0
        | VarLen _ -> failwith "VarLen not supported for numeric value"

    let getTextValue (r: RspDataRecord): string =
        match r.Value with
        | VarLen text -> text
        | _ -> getNumericValue r |> string

module MbusValueType =

    open Mbus
    open Mbus.Records
    open Mbus.Records.ValueInfoBlocks

    let toString v =
        match v with
        | MbusValueType.Energy -> "Energy"
        | MbusValueType.Date -> "Date"
        | MbusValueType.DateAndTime -> "Date And Time"
        | MbusValueType.ErrorFlags -> "Error Flags"
        | MbusValueType.ExternalTemperature -> "External Temperature"
        | MbusValueType.FabricationNumber -> "Fabrication Number"
        | MbusValueType.FlowTemperature -> "Flow Temperature"
        | MbusValueType.ReturnTemperature -> "Return Temperature"
        | MbusValueType.TemperatureDifference -> "Temperature Difference"
        | MbusValueType.MetrologyVersionNumber -> "Metrology Version Number"
        | MbusValueType.OnTime -> "On Time"
        | MbusValueType.OperatingTime -> "Operating Time"
        | MbusValueType.RemainingBatteryLifetime -> "Remaining Battery Lifetime"
        | MbusValueType.Volume -> "Volume"
        | MbusValueType.VolumeFlow -> "Volume Flow"
        | MbusValueType.VolumeFlowExt -> "Volume Flow Ext"
        | MbusValueType.Address -> "Address"
        | MbusValueType.Credit -> "Credit"
        | MbusValueType.Debit -> "Debit"
        | MbusValueType.DeviceType -> "Device Type"
        | MbusValueType.UniqueMessageIdentification -> "Unique Message Identification"
        | MbusValueType.ActualityDuration -> "Actuality Duration"
        | MbusValueType.AveragingDuration -> "Averaging Duration"
        | MbusValueType.Power -> "Power"
        | MbusValueType.HardwareVersionNumber -> "Hardware Version Number"
        | MbusValueType.DigitalInput -> "Digital Input"
        | MbusValueType.DigitalOutput -> "Digital Output"
        | MbusValueType.Customer -> "Customer"
        | MbusValueType.SpecialSupplierInformation -> "Special Supplier Information"
        | MbusValueType.MassFlow -> "Mass Flow"
        | MbusValueType.Mass -> "Mass"
        | MbusValueType.Identification -> "Identification"
        | MbusValueType.Manufacturer -> "Manufacturer"
        | MbusValueType.ParameterSetIdentification -> "Parameter Set Identification"
        | MbusValueType.Model -> "Model"
        | MbusValueType.OtherSoftwareVersionNumber -> "Other Software Version Number"
        | MbusValueType.CustomerLocation -> "Customer Location"
        | MbusValueType.AccessCodeUser -> "Access Code User"
        | MbusValueType.AccessCodeOperator -> "Access Code Operator"
        | MbusValueType.AccessCodeSystemOperator -> "Access Code System Operator"
        | MbusValueType.AccessCodeDeveloper -> "Access Code Developer"
        | MbusValueType.Password -> "Password"
        | MbusValueType.ErrorMask -> "Error Mask"
        | MbusValueType.SecurityKey -> "Security Key"
        | MbusValueType.BaudRate -> "Baud Rate"
        | MbusValueType.ResponseDelayTime -> "Response Delay Time"
        | MbusValueType.Retry -> "Retry"
        | MbusValueType.RemoteControl -> "Remote Control"
        | MbusValueType.FirstStorageNumberForCyclicStorage -> "First Storage Number For Cyclic Storage"
        | MbusValueType.LastStorageNumberForCyclicStorage -> "Last Storage Number For Cyclic Storage"
        | MbusValueType.SizeOfStorageBlock -> "Size Of Storage Block"
        | MbusValueType.StorageInterval -> "Storage Interval"
        | MbusValueType.OperatorSpecificData -> "Operator Specific Data"
        | MbusValueType.TimePointSecond -> "Time Point Second"
        | MbusValueType.DurationSinceLastReadout -> "Duration Since Last Readout"
        | MbusValueType.StartOfTariff -> "Start Of Tariff"
        | MbusValueType.PeriodOfTariff -> "Period Of Tariff"
        | MbusValueType.NoVif -> "No Vif"
        | MbusValueType.DataContainerForWMbusProtocol -> "Data Container For WMbus Protocol"
        | MbusValueType.PeriodOfNominalDataTransmissions -> "Period Of Nominal Data Transmissions"
        | MbusValueType.Voltage -> "Voltage"
        | MbusValueType.Current -> "Current"
        | MbusValueType.ResetCounter -> "Reset Counter"
        | MbusValueType.AccumulationCounter -> "Accumulation Counter"
        | MbusValueType.ControlSignal -> "Control Signal"
        | MbusValueType.DayOfWeek -> "Day Of Week"
        | MbusValueType.WeekNumber -> "Week Number"
        | MbusValueType.TimePointOfDayChange -> "Time Point Of Day Change"
        | MbusValueType.StateOfParameterActivation -> "State Of Parameter Activation"
        | MbusValueType.DurationSinceLastAccumulation -> "Duration Since Last Accumulation"
        | MbusValueType.OperatingTimeBattery -> "Operating Time Battery"
        | MbusValueType.DateAndTimeOfBatteryChange -> "Date And Time Of Battery Change"
        | MbusValueType.RfLevel -> "RF Level"
        | MbusValueType.DaylightSavings -> "Daylight Savings"
        | MbusValueType.ListeningWindowManagement -> "Listening Window Management"
        | MbusValueType.NumberOfTimesTheMeterWasStopped -> "Number Of Times The Meter Was Stopped"
        | MbusValueType.DataContainerForManufacturerSpecificProtocol -> "Data Container For Manufacturer Specific Protocol"
        | MbusValueType.ManufacturerSpecific -> "Manufacturer Specific"
        | MbusValueType.DurationOfTariff -> "Duration Of Tariff"
        | MbusValueType.ReactiveEnergy -> "Reactive Energy"
        | MbusValueType.ApparentEnergy -> "Apparent Energy"
        | MbusValueType.ReactivePower -> "Reactive Power"
        | MbusValueType.RelativeHumidity -> "Relative Humidity"
        | MbusValueType.PhaseUToU -> "Phase U to U"
        | MbusValueType.PhaseUToI -> "Phase U to I"
        | MbusValueType.Frequency -> "Frequency"
        | MbusValueType.ApparentPower -> "Apparent Power"
        | MbusValueType.ColdWarmTemperatureLimit -> "Cold/Warm Temperature Limit"
        | MbusValueType.CumulativeMaximumOfActivePower -> "Cumulative Maximum Of Active Power"
        | MbusValueType.UnitsForHca -> "Units For HCA"
        | MbusValueType.Pressure -> "Pressure"

    let fromRecord (r: RspDataRecord) =
        match r.Vib with
        | RspVib.Normal vib -> vib.Def.Val |> toString
        | RspVib.Text text -> text
        | RspVib.Mfr mfr -> $"Manufacturer Specific {mfr}"
        | RspVib.Invalid inv -> $"Invalid VIB: {inv}"

module MbusUnit =

    open Mbus
    open Mbus.Records
    open Mbus.Records.ValueInfoBlocks

    let fromRspRecord (r: RspDataRecord) =
        match r.Vib with
        | RspVib.Normal vib -> vib.Def.Unit
        | _ -> MbusUnit.NoUnit

module MbusParserError =

    open Mbus
    open Mbus.BaseParsers.Core

    let create (e: PError): MbusError =
        let msg  = $"Error while parsing {e.Ctx} at position {e.Pos} : {e.Msg}"
        MbusError(msg)
