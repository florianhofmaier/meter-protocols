namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open System

type CombVifExt =
    | AverageValue
    | InverseCompactProfile
    | RelativeDeviation
    | StandardConformDataContent
    | CompactProfileWithRegisterNumbers
    | CompactProfile
    | PerSecond
    | PerMinute
    | PerHour
    | PerDay
    | PerWeek
    | PerMonth
    | PerYear
    | PerRevolutionMeasurement
    | IncrementPerInputPulseOnChannel0
    | IncrementPerInputPulseOnChannel1
    | IncrementPerOutputPulseOnChannel0
    | IncrementPerOutputPulseOnChannel1
    | PerLitre
    | PerM3
    | PerKg
    | PerK
    | PerKWh
    | PerGJ
    | PerKW
    | PerKL
    | PerV
    | PerA
    | MultipliedByS
    | MultipliedBySV
    | MultipliedBySA
    | StartDateOf
    | VifContainsUncorrectedUnitOrValue
    | AccumulationOnlyIfPositiveContributions
    | AccumulationOfAbsValueOnlyIfNegativeContributions
    | ReservedForAlternateNonMetricUnitSystem
    | ValueAtBaseConditions
    | ObisDeclaration
    | LowerLimitValue
    | NumberOfExceedsOfLowerLimit
    | DateTimeOfBeginOfFirstLowerLimitExceed
    | DateTimeOfEndOfFirstLowerLimitExceed
    | DateTimeOfBeginOfLastLowerLimitExceed
    | DateTimeOfEndOfLastLowerLimitExceed
    | UpperLimitValue
    | NumberOfExceedsOfUpperLimit
    | DateTimeOfBeginOfFirstUpperLimitExceed
    | DateTimeOfEndOfFirstUpperLimitExceed
    | DateTimeOfBeginOfLastUpperLimitExceed
    | DateTimeOfEndOfLastUpperLimitExceed
    | MultipliedByFlowTemperatureDividedBy100
    | MultipliedByReturnTemperatureDividedBy100
    | DurationOfFirstLowerLimitExceedInSeconds
    | DurationOfFirstLowerLimitExceedInMinutes
    | DurationOfFirstLowerLimitExceedInHours
    | DurationOfFirstLowerLimitExceedInDays
    | DurationOfLastLowerLimitExceedsInSeconds
    | DurationOfLastLowerLimitExceedsInMinutes
    | DurationOfLastLowerLimitExceedsInHours
    | DurationOfLastLowerLimitExceedsInDays
    | DurationOfFirstUpperLimitExceedInSeconds
    | DurationOfFirstUpperLimitExceedInMinutes
    | DurationOfFirstUpperLimitExceedInHours
    | DurationOfFirstUpperLimitExceedInDays
    | DurationOfLastUpperLimitExceedsInSeconds
    | DurationOfLastUpperLimitExceedsInMinutes
    | DurationOfLastUpperLimitExceedsInHours
    | DurationOfLastUpperLimitExceedsInDays
    | ValueDuringLowerLimitExceed
    | ValueDuringUpperLimitExceed
    | LeakageValues
    | OverflowValues
    | MultiplicativeCorrectionFactorForValue
    | FutureValue
    | AtPhaseL1
    | AtPhaseL2
    | AtPhaseL3
    | AtNeutral
    | MeasuredFromL2ToL1
    | MeasuredFromL3ToL2
    | MeasuredFromL1ToL3
    | AtQuadrantQ1
    | AtQuadrantQ2
    | AtQuadrantQ3
    | AtQuadrantQ4
    | DeltaBetweenImportAndExport
    | UsedForAlternateNonMetricUnitSystem
    | MeasurementRelatedToASecondarySensorOfTheDevice
    | AdditionalRegisterVariantWithHigherResolutionThanTheMainRegisterOfTheMeter
    | AccumulationOfAbsoluteValueForBothPositiveAndNegativeContributionAbsoluteCount
    | DataPresentedWithTypeC
    | DataPresentedWithTypeD
    | EndDateTime
    | DirectionFromCommunicationPartnerToMeter
    | DirectionFromMeterToCommunicationPartner
    | LiquidPressure
    | GasPressure
    | ValueMeasuredOutdoors
    | MeasuredFromL1ToL2
    | MeasuredFromL2ToL3
    | MeasuredFromL3ToL1

type VifExtension =
    | CombVifExt of CombVifExt
    | MfrVifExt of ReadOnlyMemory<uint8>

module VifExtensions =

    let private primTable =
        Map [
            0x12uy, CombVifExt.AverageValue
            0x13uy, CombVifExt.InverseCompactProfile
            0x14uy, CombVifExt.RelativeDeviation
            0x1Duy, CombVifExt.StandardConformDataContent
            0x1Euy, CombVifExt.CompactProfileWithRegisterNumbers
            0x1Fuy, CombVifExt.CompactProfile
            0x20uy, CombVifExt.PerSecond
            0x21uy, CombVifExt.PerMinute
            0x22uy, CombVifExt.PerHour
            0x23uy, CombVifExt.PerDay
            0x24uy, CombVifExt.PerWeek
            0x25uy, CombVifExt.PerMonth
            0x26uy, CombVifExt.PerYear
            0x27uy, CombVifExt.PerRevolutionMeasurement
            0x28uy, CombVifExt.IncrementPerInputPulseOnChannel0
            0x29uy, CombVifExt.IncrementPerInputPulseOnChannel1
            0x2Auy, CombVifExt.IncrementPerOutputPulseOnChannel0
            0x2Buy, CombVifExt.IncrementPerOutputPulseOnChannel1
            0x2Cuy, CombVifExt.PerLitre
            0x2Duy, CombVifExt.PerM3
            0x2Euy, CombVifExt.PerKg
            0x2Fuy, CombVifExt.PerK
            0x30uy, CombVifExt.PerKWh
            0x31uy, CombVifExt.PerGJ
            0x32uy, CombVifExt.PerKW
            0x33uy, CombVifExt.PerKL
            0x34uy, CombVifExt.PerV
            0x35uy, CombVifExt.PerA
            0x36uy, CombVifExt.MultipliedByS
            0x37uy, CombVifExt.MultipliedBySV
            0x38uy, CombVifExt.MultipliedBySA
            0x39uy, CombVifExt.StartDateOf
            0x3Auy, CombVifExt.VifContainsUncorrectedUnitOrValue
            0x3Buy, CombVifExt.AccumulationOnlyIfPositiveContributions
            0x3Cuy, CombVifExt.AccumulationOfAbsValueOnlyIfNegativeContributions
            0x3Duy, CombVifExt.ReservedForAlternateNonMetricUnitSystem
            0x3Euy, CombVifExt.ValueAtBaseConditions
            0x3Fuy, CombVifExt.ObisDeclaration
            0x40uy, CombVifExt.LowerLimitValue
            0x41uy, CombVifExt.NumberOfExceedsOfLowerLimit
            0x42uy, CombVifExt.DateTimeOfBeginOfFirstLowerLimitExceed
            0x43uy, CombVifExt.DateTimeOfEndOfFirstLowerLimitExceed
            0x46uy, CombVifExt.DateTimeOfBeginOfLastLowerLimitExceed
            0x47uy, CombVifExt.DateTimeOfEndOfLastLowerLimitExceed
            0x48uy, CombVifExt.UpperLimitValue
            0x49uy, CombVifExt.NumberOfExceedsOfUpperLimit
            0x4Auy, CombVifExt.DateTimeOfBeginOfFirstUpperLimitExceed
            0x4Buy, CombVifExt.DateTimeOfEndOfFirstUpperLimitExceed
            0x4Euy, CombVifExt.DateTimeOfBeginOfLastUpperLimitExceed
            0x4Fuy, CombVifExt.DateTimeOfEndOfLastUpperLimitExceed
            0x44uy, CombVifExt.MultipliedByFlowTemperatureDividedBy100
            0x45uy, CombVifExt.MultipliedByReturnTemperatureDividedBy100
            0x50uy, CombVifExt.DurationOfFirstLowerLimitExceedInSeconds
            0x51uy, CombVifExt.DurationOfFirstLowerLimitExceedInMinutes
            0x52uy, CombVifExt.DurationOfFirstLowerLimitExceedInHours
            0x53uy, CombVifExt.DurationOfFirstLowerLimitExceedInDays
            0x54uy, CombVifExt.DurationOfLastLowerLimitExceedsInSeconds
            0x55uy, CombVifExt.DurationOfLastLowerLimitExceedsInMinutes
            0x56uy, CombVifExt.DurationOfLastLowerLimitExceedsInHours
            0x57uy, CombVifExt.DurationOfLastLowerLimitExceedsInDays
            0x58uy, CombVifExt.DurationOfFirstUpperLimitExceedInSeconds
            0x59uy, CombVifExt.DurationOfFirstUpperLimitExceedInMinutes
            0x5Auy, CombVifExt.DurationOfFirstUpperLimitExceedInHours
            0x5Buy, CombVifExt.DurationOfFirstUpperLimitExceedInDays
            0x5Cuy, CombVifExt.DurationOfLastUpperLimitExceedsInSeconds
            0x5Duy, CombVifExt.DurationOfLastUpperLimitExceedsInMinutes
            0x5Euy, CombVifExt.DurationOfLastUpperLimitExceedsInHours
            0x5Fuy, CombVifExt.DurationOfLastUpperLimitExceedsInDays
            0x68uy, CombVifExt.ValueDuringLowerLimitExceed
            0x6Cuy, CombVifExt.ValueDuringUpperLimitExceed
            0x69uy, CombVifExt.LeakageValues
            0x6Duy, CombVifExt.OverflowValues
            0x7Duy, CombVifExt.MultiplicativeCorrectionFactorForValue
            0x7Euy, CombVifExt.FutureValue
        ]

    let private extTable =
        Map [
            0x01uy, CombVifExt.AtPhaseL1
            0x02uy, CombVifExt.AtPhaseL2
            0x03uy, CombVifExt.AtPhaseL3
            0x04uy, CombVifExt.AtNeutral
            0x05uy, CombVifExt.MeasuredFromL2ToL1
            0x06uy, CombVifExt.MeasuredFromL3ToL2
            0x07uy, CombVifExt.MeasuredFromL1ToL3
            0x08uy, CombVifExt.AtQuadrantQ1
            0x09uy, CombVifExt.AtQuadrantQ2
            0x0Auy, CombVifExt.AtQuadrantQ3
            0x0Buy, CombVifExt.AtQuadrantQ4
            0x0Cuy, CombVifExt.DeltaBetweenImportAndExport
            0x0Duy, CombVifExt.UsedForAlternateNonMetricUnitSystem
            0x0Euy, CombVifExt.MeasurementRelatedToASecondarySensorOfTheDevice
            0x0Fuy, CombVifExt.AdditionalRegisterVariantWithHigherResolutionThanTheMainRegisterOfTheMeter
            0x10uy, CombVifExt.AccumulationOfAbsoluteValueForBothPositiveAndNegativeContributionAbsoluteCount
            0x11uy, CombVifExt.DataPresentedWithTypeC
            0x12uy, CombVifExt.DataPresentedWithTypeD
            0x13uy, CombVifExt.EndDateTime
            0x14uy, CombVifExt.DirectionFromCommunicationPartnerToMeter
            0x15uy, CombVifExt.DirectionFromMeterToCommunicationPartner
            0x16uy, CombVifExt.LiquidPressure
            0x17uy, CombVifExt.GasPressure
            0x18uy, CombVifExt.ValueMeasuredOutdoors
            0x25uy, CombVifExt.MeasuredFromL1ToL2
            0x26uy, CombVifExt.MeasuredFromL2ToL3
            0x27uy, CombVifExt.MeasuredFromL3ToL1
        ]

    let private extensionOfVifeCode = 0x7Cuy
    let private mfrSpecificExt = 0x7Fuy

    let private tryMapPrim =
        Utility.tryMap primTable

    let private tryMapExt =
        Utility.tryMap extTable

    let private mapMfrExt (bytes: ReadOnlyMemory<byte>) pos =
        Some (MfrVifExt (bytes.Slice(pos))), bytes.Span.Length

    let tryMap
        (bytes: ReadOnlyMemory<byte>)
        (pos: int)
        : VifExtension option * int =

        let asCombVifExt (vifExt, p) = Option.map VifExtension.CombVifExt vifExt, p

        if pos >= bytes.Length then
            None, pos
        else
            let code = Utility.maskCode bytes pos

            match code with
            | _ when code = extensionOfVifeCode ->
                if pos + 1 >= bytes.Length then
                    None, bytes.Length
                else
                    match tryMapExt bytes (pos + 1) with
                    | Some ext, nextPos -> Some (VifExtension.CombVifExt ext), nextPos
                    | None, _ -> None, min (pos + 2) bytes.Length

            | _ when code = mfrSpecificExt ->
                mapMfrExt bytes (pos + 1)

            | _ ->
                tryMapPrim bytes pos
                |> asCombVifExt
