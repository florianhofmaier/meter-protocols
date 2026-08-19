namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Common.Security.Cryptography.AesCbc
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

module LongHeaderMode5Raw =

    let numberOfEncryptedBytes header =
        ConfigurationFieldBitsRaw.value header.Cnf.Value
        |> NumberOfEncryptedBlocks.map
        |> NumberOfEncryptedBlocks.value
        |> (*) AesCbc.blockLength

type LongHeaderRaw =
    | Mode0Raw of LongHeaderMode0Raw
    | Mode5Raw of LongHeaderMode5Raw

module LongHeaderRaw =

    let numOfEncryptedBytes header =
        match header with
        | Mode0Raw _ ->
            0
        | Mode5Raw mode5 ->
            LongHeaderMode5Raw.numberOfEncryptedBytes mode5

    let parse : Parser<Field<LongHeaderRaw>> =
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
                return Mode0Raw {
                    IdNum = idNum; Mfr = mfr; Version = version; DevType = devType
                    Acc = acc; Status = status; Cnf = bits
                }

            | ConfigurationFieldRaw.Mode5Raw bits ->
                return Mode5Raw {
                    IdNum = idNum; Mfr = mfr; Version = version; DevType = devType
                    Acc = acc; Status = status; Cnf = bits
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
        : Validation<LongHeader> =

        validator {
            match raw.Value with
            | Mode0Raw header ->
                let! device = DeviceIdentification.fromRawElements header.IdNum header.Mfr header.Version header.DevType
                and! acc = validateField AccessNumber.fromRaw header.Acc
                and! status = validateField StatusByte.fromRaw header.Status
                and! cnf = validateField ConfigurationFieldMode0.fromRaw header.Cnf
                return Mode0 { Device = device; Acc = acc; Status = status; Cnf = cnf }

            | Mode5Raw header ->
                let! device = DeviceIdentification.fromRawElements header.IdNum header.Mfr header.Version header.DevType
                and! acc = validateField AccessNumber.fromRaw header.Acc
                and! status = validateField StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return Mode5 { Device = device; Acc = acc; Status = status; Cnf = cnf }

        }
