namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.BaseWriters.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Utility.Result

/// Meter identity without parser provenance, shared by messages and key selection.
type MeterAddress = {
    IdNum: IdNumber
    Mfr: Manufacturer
    Version: Version
    DevType: DeviceType
}

module MeterAddress =

    /// Creates a configured identity without parser fields or source provenance.
    let create idNumber manufacturer version deviceType : Result<MeterAddress, string> =
        result {
            let! id = IdNumber.create idNumber
            let! mfr = Manufacturer.create manufacturer
            let! ver = Version.create version
            let! typ = DeviceType.create deviceType

            return {
                IdNum = id
                Mfr = mfr
                Version = ver
                DevType = typ
            }
        }

    /// Writes the meter identification fields in standard order.
    let write (address: MeterAddress) =
        writer {
            do! IdNumber.write address.IdNum
            do! Manufacturer.write address.Mfr
            do! Version.write address.Version
            do! DeviceType.write address.DevType
        }

type DeviceIdentificationRaw = {
    IdNum: Field<IdNumberRaw>
    Mfr: Field<ManufacturerRaw>
    Version: Field<VersionRaw>
    DevType: Field<DeviceTypeRaw>
}

type DeviceIdentification = {
    IdNum: Field<IdNumber>
    Mfr: Field<Manufacturer>
    Version: Field<Version>
    DevType: Field<DeviceType>
}



module DeviceIdentification =

    let validateElements
        (idNum: Field<IdNumberRaw>)
        (mfr: Field<ManufacturerRaw>)
        (version: Field<VersionRaw>)
        (devType: Field<DeviceTypeRaw>)
        : Validation<DeviceIdentification> =

        validator {
            let! idNum = IdNumber.validate idNum
            and! mfr = Manufacturer.validate mfr
            and! version = Version.validate version
            and! devType = DeviceType.validate devType

            return {
                IdNum = idNum
                Mfr = mfr
                Version = version
                DevType = devType
            }
        }

    let validate (raw: DeviceIdentificationRaw) : Validation<DeviceIdentification> =

        validateElements raw.IdNum raw.Mfr raw.Version raw.DevType

    /// Returns the semantic meter identity without field IDs or spans.
    let toMeterAddress (device: DeviceIdentification) : MeterAddress = {
        IdNum = device.IdNum.Value
        Mfr = device.Mfr.Value
        Version = device.Version.Value
        DevType = device.DevType.Value
    }

    /// Writes the four device-identification fields in their standard order.
    let write device =
        MeterAddress.write (toMeterAddress device)
