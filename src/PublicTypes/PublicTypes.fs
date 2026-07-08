namespace Metering.Mbus

open System

type MbusError(msg: string) =
    inherit Exception(msg)

type MbusDeviceType =
    | Other = 0x00
    | OilMeter = 0x01
    | ElectricityMeter = 0x02
    | GasMeter = 0x03
    | HeatMeter = 0x04
    | SteamMeter = 0x05
    | WarmWaterMeter = 0x06
    | WaterMeter = 0x07
    | HeatCostAllocator = 0x08
    | CompressedAir = 0x09
    | CoolingMeterReturn = 0x0A
    | CoolingMeterFlow = 0x0B
    | HeatMeterFlow = 0x0C
    | CombinedHeatCoolingMeter = 0x0D
    | BusSystemComponent = 0x0E
    | Unknown = 0x0F
    | CalorificValue = 0x14
    | HotWaterMeter = 0x15
    | ColdWaterMeter = 0x16
    | DualRegisterWaterMeter = 0x17
    | PressureMeter = 0x18
    | ADConverter = 0x19
    | SmokeDetector = 0x1A
    | RoomSensor = 0x1B
    | GasDetector = 0x1C
    | Breaker = 0x20
    | Valve = 0x21
    | WasteWaterMeter = 0x28
    | Garbage = 0x29
    | CommunicationController = 0x31
    | UnidirectionalRepeater = 0x32
    | BidirectionalRepeater = 0x33

type MbusAddress = { IdNumber: int; Mfr: string; Version: int; DeviceType: MbusDeviceType}

module MbusAddress =
    let create id mfr version deviceType : Result<MbusAddress, string> =
        if id < 0 || id > 99999999 then
            Error $"Invalid ID number: {id}, must be between 0 and 99999999"
        elif String.length mfr<> 3 then
            Error $"Invalid manufacturer code length: {mfr.Length}, expected 3"
        elif not (mfr |> Seq.forall (fun c -> c >= 'A' && c <= 'Z')) then
            Error $"Invalid manufacturer code: {mfr}, must contain only uppercase letters A-Z"
        elif version < 0 || version > 255 then
            Error $"Invalid version: {version}, must be between 0 and 255"
        else Ok { IdNumber = id; Mfr = mfr; Version = version; DeviceType = deviceType }

type MbusApplicationError =
    | NoError = 0uy
    | ApplicationBusy = 1uy
    | AnyApplicationError = 2uy
    | AbnormalCondition = 3uy

type MbusStatusField =
    { ApplicationError : MbusApplicationError
      PowerLow : bool
      PermanentError : bool
      TemporaryError : bool }

    static member CreateEmpty=
        { ApplicationError = MbusApplicationError.NoError
          PowerLow = false
          PermanentError = false
          TemporaryError = false }

type MbusDataType =
    | NoData
    | Bcd2Digit
    | Bcd4Digit
    | Bcd6Digit
    | Bcd8Digit
    | Bcd12Digit
    | Integer8Bit
    | Integer16Bit
    | Integer24Bit
    | Integer32Bit
    | Integer48Bit
    | Integer64Bit
    | Real32Bit
    | Text

/// <summary>
/// Represents the Function Field of the Data Information Block (DIB).
/// It gives the type of value as specified in EN 13757-3.
/// </summary>
type MbusFunctionField =
    | InstValue = 0
    | MaxValue = 1
    | MinValue = 2
    | ValueInErrorState = 3

type MbusValueType =
    | Energy
    | Date
    | DateAndTime
    | ErrorFlags
    | ExternalTemperature
    | FabricationNumber
    | FlowTemperature
    | ReturnTemperature
    | TemperatureDifference
    | MetrologyVersionNumber
    | OnTime
    | OperatingTime
    | RemainingBatteryLifetime
    | Volume
    | VolumeFlow
    | VolumeFlowExt
    | Address
    | Credit
    | Debit
    | DeviceType
    | UniqueMessageIdentification
    | ActualityDuration
    | AveragingDuration
    | Power
    | HardwareVersionNumber
    | DigitalInput
    | DigitalOutput
    | Customer
    | SpecialSupplierInformation
    | MassFlow
    | Mass
    | Identification
    | Manufacturer
    | ParameterSetIdentification
    | Model
    | OtherSoftwareVersionNumber
    | CustomerLocation
    | AccessCodeUser
    | AccessCodeOperator
    | AccessCodeSystemOperator
    | AccessCodeDeveloper
    | Password
    | ErrorMask
    | SecurityKey
    | BaudRate
    | ResponseDelayTime
    | Retry
    | RemoteControl
    | FirstStorageNumberForCyclicStorage
    | LastStorageNumberForCyclicStorage
    | SizeOfStorageBlock
    | StorageInterval
    | OperatorSpecificData
    | TimePointSecond
    | DurationSinceLastReadout
    | StartOfTariff
    | PeriodOfTariff
    | NoVif
    | DataContainerForWMbusProtocol
    | PeriodOfNominalDataTransmissions
    | Voltage
    | Current
    | ResetCounter
    | AccumulationCounter
    | ControlSignal
    | DayOfWeek
    | WeekNumber
    | TimePointOfDayChange
    | StateOfParameterActivation
    | DurationSinceLastAccumulation
    | OperatingTimeBattery
    | DateAndTimeOfBatteryChange
    | RfLevel
    | DaylightSavings
    | ListeningWindowManagement
    | NumberOfTimesTheMeterWasStopped
    | DataContainerForManufacturerSpecificProtocol
    | DataContainerForMbusUpperLayers
    | ManufacturerSpecific
    | DurationOfTariff
    | ReactiveEnergy
    | ApparentEnergy
    | ReactivePower
    | RelativeHumidity
    | PhaseUToU
    | PhaseUToI
    | Frequency
    | ApparentPower
    | ColdWarmTemperatureLimit
    | CumulativeMaximumOfActivePower
    | UnitsForHca
    | Pressure
    | CurrentSelectedApplication
    | SubDeviceType
    | NumberOfAvailableCommunicationCreditsOnTheLocalInterface
    | NumberOfAvailableCommunicationCreditsOnTheWirelessMbusInterface
    | InstallationConditions
    | Co2Content
    | CoContent
    | VocContent
    | ParticlesUnspecifiedRange
    | ParticlesPm1
    | ParticlesPm2_5
    | ParticlesPm10
    | Illuminance
    | LuminousIntensity
    | Irradiance
    | WindSpeed
    | Rainfall
    | Noise
    | Turbidity
    | PhValue
    | NumberOfDismounts
    | NumberOfTestButtonOperatedCounter
    | NumberOfAlarms
    | NumberOfAlarmMuteSwitchOperatedCounter
    | NumberOfObstacleDetectedCounter
    | SmokeEntriesBlockingCumulatedCounter
    | SmokeChamberDefectCumulatedCounter
    | NumberOfSelfTestFailedCounter
    | NumberOfSounderDefectCounter
    | NumberOfCoAlarmsLowLevel
    | NumberOfCoAlarmsMediumLevel
    | NumberOfCoAlarmsHighLevel
    | BatteryStatus
    | ChamberPollutionLevel
    | Distance
    | MoistureLevel
    | TypeOrClassOfApproval
    | StatusBitsForPressureDevices
    | StatusBitsForSmokeAlarmDevices
    | StatusBitsForCoAlarmDevices
    | StatusBitsForHeatAlarmDevices
    | StatusBitsForDoorContactSensorAndLockedDoorDetector

type MbusActionCode =
    | Set = 0x00uy
    | AddValue = 0x01uy
    | SubtractValue = 0x02uy
    | Or = 0x03uy
    | And = 0x04uy
    | Xor = 0x05uy
    | AndNot = 0x06uy
    | Clear = 0x07uy
    | AddEntry = 0x08uy
    | DeleteEntry = 0x09uy
    | DelayedAction = 0x0Auy
    | FreezeData = 0x0Buy
    | AddToReadoutList = 0x0Duy
    | DeleteFromReadoutList = 0x0Euy
    | Get = 0x0Fuy

type MbusRecordError =
    | NoError = 0x00
    | TooManyDifes = 0x01
    | StorageNumberNotImplemented = 0x02
    | UnitNumberNotImplemented = 0x03
    | TariffNumberNotImplemented = 0x04
    | FunctionNotImplemented = 0x05
    | DataClassNotImplemented = 0x06
    | DataSizeNotImplemented = 0x07
    | TooManyVifes = 0x0B
    | IllegalVifGroup = 0x0C
    | IllegalVifExponent = 0x0D
    | VifDifMismatch = 0x0E
    | UnimplementedAction = 0x0F
    | NoDataAvailable = 0x15
    | DataOverflow = 0x16
    | DataUnderflow = 0x17
    | DataError = 0x18
    | PrematureEndOfRecord = 0x1C

type MbusValueTypeExtension =
    | AverageValue = 0x12uy
    | InverseCompactProfile = 0x13uy
    | RelativeDeviation = 0x14uy
    | StandardConformDataContent = 0x1Duy
    | CompactProfileWithRegisterNumbers = 0x1Euy
    | CompactProfile = 0x1Fuy
    | PerSecond = 0x20uy
    | PerMinute = 0x21uy
    | PerHour = 0x22uy
    | PerDay = 0x23uy
    | PerWeek = 0x24uy
    | PerMonth = 0x25uy
    | PerYear = 0x26uy
    | PerRevolutionMeasurement = 0x27uy
    | IncrementPerInputPulseOnChannel0 = 0x28uy
    | IncrementPerInputPulseOnChannel1 = 0x29uy
    | IncrementPerOutputPulseOnChannel0 = 0x2Auy
    | IncrementPerOutputPulseOnChannel1 = 0x2Buy
    | PerLitre = 0x2Cuy
    | PerM3 = 0x2Duy
    | PerKg = 0x2Euy
    | PerK = 0x2Fuy
    | PerKWh = 0x30uy
    | PerGJ = 0x31uy
    | PerKW = 0x32uy
    | PerKL = 0x33uy
    | PerV = 0x34uy
    | PerA = 0x35uy
    | MultipliedByS = 0x36uy
    | MultipliedBySV = 0x37uy
    | MultipliedBySA = 0x38uy
    | StartDateOf = 0x39uy
    | VifContainsUncorrectedUnitOrValue = 0x3Auy
    | AccumulationOnlyIfPositiveContributions = 0x3Buy
    | AccumulationOfAbsValueOnlyIfNegativeContributions = 0x3Cuy
    | ReservedForAlternateNonMetricUnitSystem = 0x3Duy
    | ValueAtBaseConditions = 0x3Euy
    | ObisDeclaration = 0x3Fuy
    | MultiplicativeCorrectionFactorForValue = 0x7Duy
    | FutureValue = 0x7Euy
    | ManufacturerSpecific = 0x7Fuy

type MbusUnit =
    | NoUnit
    | WattHours
    | Watts
    | JoulesPerHour
    | Joules
    | Calories
    | Celsius
    | Kelvin
    | CubicMeters
    | CubicMetersPerSecond
    | CubicMetersPerMinute
    | CubicMetersPerHour
    | Degrees
    | Hertz
    | KiloGrams
    | KiloGramsPerHour
    | Tons
    | Seconds
    | Days
    | Minutes
    | Hours
    | KiloBitsPerSecond
    | Months
    | Years
    | Volts
    | Amps
    | DecibelMilliWatts
    | VoltAmpereReactiveHours
    | VoltAmpereHours
    | VoltAmpereReactive
    | VoltAmpere
    | Percent
    | Ppm
    | Ppb
    | CubicFeet
    | Bar
    | MicroGramsPerCubicMeter
    | MillionUnitsPerCubicMeter
    | Lux
    | Candela
    | WattsPerSquareMeter
    | KilometersPerHour
    | LiterPerSquareMillimeter
    | DecibelAWeighted
    | FormazinNephelometricUnit
    | Millimeter

// module MbusUnit =
//     let toText unit =
//         match unit with
//         | NoUnit -> ""
//         | WattHours -> "Wh"
//         | Watt -> "W"
//         | JoulesPerHour -> "J/h"
//         | Joules -> "J"
//         | Celsius -> "°C"
//         | CubicMeters -> "m³"
//         | CubicMetersPerSecond -> "m³/s"
//         | CubicMetersPerMinute -> "m³/minutes"
//         | CubicMetersPerHour -> "m³/h"
//         | Kelvin -> "K"
//         | KiloGrams -> "kg"
//         | KiloGramsPerHour -> "kg/h"
//         | Tons -> "t"
//         | Seconds -> "s"
//         | Days -> "d"
//         | Minutes -> "minutes"
//         | Hours -> "h"
//         | KiloBitsPerSecond -> "kbps"
//         | Months -> "months"
//         | Years -> "years"
//         | Volts -> "V"
//         | Amps -> "A"
//         | DecibelMilliWatts -> "dBm"
//         | Calories -> "Cal"
//         | VoltAmpereReactiveHours -> "VARh"
//         | VoltAmpereHours -> "VAh"
//         | VoltAmpereReactive -> "VAR"
//         | VoltAmpere -> "VA"
//         | Percent -> "%"
//         | CubicFeet -> "feet³"
//         | Degrees -> "°"
//         | Hertz -> "Hz"
//         | Bar -> "bar"

[<AbstractClass>]
type MbusRecordBase(unit: MbusUnit, fn: MbusFunctionField, storageNum: int, tariff: int, subUnit: int) =
    member _.Unit = unit
    member _.Function = fn
    member _.StorageNumber = storageNum
    member _.Tariff = tariff
    member _.SubUnit = subUnit

type MbusNumericalRecord(unit: MbusUnit, fn: MbusFunctionField, storageNum: int, tariff: int, subUnit: int, value: double, valueType: MbusValueType) =
    inherit MbusRecordBase(unit, fn, storageNum, tariff, subUnit)
    member _.Value = value
    member _.ValueType = valueType

type MbusTextRecord(unit: MbusUnit, fn: MbusFunctionField, storageNum: int, tariff: int, subUnit: int, value: string, valueType: string) =
    inherit MbusRecordBase(unit, fn, storageNum, tariff, subUnit)
    member _.Value = value
    member _.ValueType = valueType