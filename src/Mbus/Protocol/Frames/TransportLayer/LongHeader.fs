namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DeviceIdentification

type LongHeaderMode0Raw =
    {
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
    }

type LongHeaderMode5Raw =
    {
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
        Verification: Field<DecryptionVerificationRaw>
    }

type LongHeaderOtherModeRaw =
    {
        Mode: byte
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
    }

type LongHeaderRaw =
    | Mode0Raw of LongHeaderMode0Raw
    | Mode5Raw of LongHeaderMode5Raw
    | OtherModeRaw of LongHeaderOtherModeRaw

module LongHeaderRaw =

    let parse : Parser<Field<LongHeaderRaw>> =
        parseField "Long Tpl Header"
        <| parser {
            let! idNum = IdNumberRaw.parse
            let! mfr = ManufacturerRaw.parse
            let! version = VersionRaw.parse
            let! devType = DeviceTypeRaw.parse
            let! acc = AccessNumberRaw.parse
            let! status = StatusByteRaw.parse
            let! cnf = ConfigFieldRaw.parse

            match cnf.Value with
            | ConfigFieldRaw.Mode0Raw bits ->
                return
                    Mode0Raw
                        {
                            IdNum = idNum
                            Mfr = mfr
                            Version = version
                            DevType = devType
                            Acc = acc
                            Status = status
                            Cnf = bits
                        }

            | ConfigFieldRaw.Mode5Raw bits ->
                let! verification = DecryptionVerificationRaw.parse
                return
                    Mode5Raw
                        {
                            IdNum = idNum
                            Mfr = mfr
                            Version = version
                            DevType = devType
                            Acc = acc
                            Status = status
                            Cnf = bits
                            Verification = verification
                        }
        }

type LongHeaderMode0 =
    {
        Device: DeviceIdentification
        Acc: Field<AccessNumber>
        Status: Field<StatusByte>
        Cnf: Field<ConfigurationFieldMode0>
    }

type LongHeaderMode5 =
    {
        Device: DeviceIdentification
        Acc: Field<AccessNumber>
        Status: Field<StatusByte>
        Cnf: Field<ConfigurationFieldMode5>
    }

type LongHeader =
    | Mode0 of LongHeaderMode0
    | Mode5 of LongHeaderMode5

module LongHeader =

    let fromRaw
        (raw: Field<LongHeaderRaw>)
        : Validation<Field<LongHeader>> =

        validator {
            match raw.Value with
            | Mode0Raw header ->
                let! device =
                    DeviceIdentification.fromRaw
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType
                and! acc = AccessNumber.fromRaw header.Acc
                and! status = StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode0.fromRaw header.Cnf
                return
                    Mode0 {
                        Device = device
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }
                    |> Field.withValue raw

            | Mode5Raw header ->
                let! device =
                    DeviceIdentification.fromRaw
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType
                and! acc = AccessNumber.fromRaw header.Acc
                and! status = StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    Mode5 {
                        Device = device
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }
                    |> Field.withValue raw

            | OtherModeRaw header ->
                let! _device =
                    DeviceIdentification.fromRaw
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType
                and! _acc = AccessNumber.fromRaw header.Acc
                and! _status = StatusByte.fromRaw header.Status
                and! unsupported : Field<LongHeader> =
                    failed
                        header.Cnf
                        (Mode.unsupportedMessage header.Mode)

                return unsupported
        }
