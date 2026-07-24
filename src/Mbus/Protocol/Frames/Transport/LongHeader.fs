namespace Metering.Mbus.Protocol.Frames.Transport

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
        Cnf: Field<ConfigurationFieldBitsRaw>
    }

type LongHeaderMode5Raw =
    {
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigurationFieldBitsRaw>
    }

type LongHeaderRaw =
    | Mode0Raw of LongHeaderMode0Raw
    | Mode5Raw of LongHeaderMode5Raw

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
            let! cnf = ConfigurationFieldRaw.parse

            match cnf.Value with
            | ConfigurationFieldRaw.Mode0Raw bits ->
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

            | ConfigurationFieldRaw.Mode5Raw bits ->
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
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode0.fromRaw header.Cnf
                return
                    raw
                    |> Field.withValue (
                        Mode0 {
                            Device = device
                            Acc = acc
                            Status = status
                            Cnf = cnf
                        }
                    )

            | Mode5Raw header ->
                let! device =
                    DeviceIdentification.fromRaw
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    raw
                    |> Field.withValue (
                        Mode5 {
                            Device = device
                            Acc = acc
                            Status = status
                            Cnf = cnf
                        }
                    )
        }
