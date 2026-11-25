namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.DeviceIdentification

type LongHeaderMode0Raw = {
    IdNum: Field<IdNumberRaw>
    Mfr: Field<ManufacturerRaw>
    Version: Field<VersionRaw>
    DevType: Field<DeviceTypeRaw>
    Acc: Field<AccessNumberRaw>
    Status: Field<StatusByteRaw>
    Cnf: Field<ConfigurationFieldBitsRaw>
}

type LongHeaderMode5Raw = {
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

    let parse: Parser<Field<LongHeaderRaw>> =
        parseField "Long TPL Header"
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
                    Mode0Raw {
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
                    Mode5Raw {
                        IdNum = idNum
                        Mfr = mfr
                        Version = version
                        DevType = devType
                        Acc = acc
                        Status = status
                        Cnf = bits
                    }
        }

type LongHeaderMode0 = {
    Device: DeviceIdentification
    Acc: Field<AccessNumber>
    Status: Field<StatusByte>
    Cnf: Field<ConfigurationFieldMode0>
}

type LongHeaderMode5 = {
    Device: DeviceIdentification
    Acc: Field<AccessNumber>
    Status: Field<StatusByte>
    Cnf: Field<ConfigurationFieldMode5>
}

type LongHeader =
    | Mode0 of LongHeaderMode0
    | Mode5 of LongHeaderMode5

module LongHeader =

    /// EN 13757-7:2018, Table 12: identity (8), access (1), status (1), configuration (2).
    [<Literal>]
    let mode0Length = 12

    let validate (raw: Field<LongHeaderRaw>) : Validation<LongHeader> =

        validator {
            match raw.Value with
            | Mode0Raw header ->
                let! device =
                    DeviceIdentification.validateElements
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType

                and! acc = validateField AccessNumber.validate header.Acc
                and! status = validateField StatusByte.validate header.Status
                and! cnf = validateField ConfigurationFieldMode0.validate header.Cnf

                return
                    Mode0 {
                        Device = device
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }

            | Mode5Raw header ->
                let! device =
                    DeviceIdentification.validateElements
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType

                and! acc = validateField AccessNumber.validate header.Acc
                and! status = validateField StatusByte.validate header.Status
                and! cnf = ConfigurationFieldMode5.validate header.Cnf

                return
                    Mode5 {
                        Device = device
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }

        }

    /// Writes the long TPL header's ordered fields without requiring parser provenance.
    let internal writeFields device access status configuration : Writer<unit> =
        writer {
            do! device
            do! AccessNumber.write access
            do! status
            do! configuration
        }

    /// Writes a mode-0 long header from semantic values without parser provenance.
    let writeMode0 meter access status configuration =
        writeFields
            (MeterAddress.write meter)
            access
            (StatusByte.write status)
            (ConfigurationFieldMode0.write configuration)

    /// Writes a mode-5 long header from semantic values; payload protection is separate.
    let writeMode5 meter access status configuration =
        writeFields
            (MeterAddress.write meter)
            access
            (StatusByte.write status)
            (ConfigurationFieldMode5.write configuration)

    /// Writes a wired partner's mode-0 long header (EN 13757-7:2018, 7.5.7, Tables 16/17).
    let writeCommandMode0 meter access configuration =
        writeFields
            (MeterAddress.write meter)
            access
            (writeU8 0uy)
            (ConfigurationFieldMode0.write configuration)

    /// Writes a wired partner's mode-5 long header; payload protection is separate.
    let writeCommandMode5 meter access configuration =
        writeFields
            (MeterAddress.write meter)
            access
            (writeU8 0uy)
            (ConfigurationFieldMode5.write configuration)

    /// Writes the validated LongHeader fields; protection is a separate operation.
    let write (header: LongHeader) : Writer<unit> =
        match header with
        | LongHeader.Mode0 header ->
            writeFields
                (DeviceIdentification.write header.Device)
                header.Acc.Value
                (StatusByte.write header.Status.Value)
                (ConfigurationFieldMode0.write header.Cnf.Value)
        | LongHeader.Mode5 header ->
            writeFields
                (DeviceIdentification.write header.Device)
                header.Acc.Value
                (StatusByte.write header.Status.Value)
                (ConfigurationFieldMode5.write header.Cnf.Value)
