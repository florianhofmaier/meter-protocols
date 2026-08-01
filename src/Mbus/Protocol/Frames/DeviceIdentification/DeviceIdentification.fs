namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

type DeviceIdentificationRaw =
    {
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
    }

type DeviceIdentification =
    {
        IdNum: Field<IdNumber>
        Mfr: Field<Manufacturer>
        Version: Field<Version>
        DevType: Field<DeviceType>
    }



module DeviceIdentification =

    let fromRawElements
        (idNum: Field<IdNumberRaw>)
        (mfr: Field<ManufacturerRaw>)
        (version: Field<VersionRaw>)
        (devType: Field<DeviceTypeRaw>)
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

    let fromRaw (raw: DeviceIdentificationRaw)
        : Validation<DeviceIdentification> =

        fromRawElements raw.IdNum raw.Mfr raw.Version raw.DevType