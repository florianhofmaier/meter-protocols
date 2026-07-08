namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

type DeviceIdentification =
    {
        IdNum: IdNumber
        Mfr: Manufacturer
        Version: Version
        DevType: DeviceType
    }

module DeviceIdentification =

    let fromRaw
        (idNum: ParsedField<IdNumberRaw>)
        (mfr: ParsedField<ManufacturerRaw>)
        (version: ParsedField<VersionRaw>)
        (devType: ParsedField<DeviceTypeRaw>)
        : Validation<DeviceIdentification> =

        validator {
            let! idNum = IdNumber.fromRaw idNum
            and! mfr = Manufacturer.fromRaw mfr
            and! version = Version.fromRaw version
            and! devType = DeviceType.fromRaw devType

            return
                {
                    IdNum = idNum
                    Mfr = mfr
                    Version = version
                    DevType = devType
                }
        }

