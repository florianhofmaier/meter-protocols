namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type DeviceTypeRaw = private RawDeviceType of uint8

type DeviceType = private DeviceType of uint8

module DeviceTypeRaw =

    let value (RawDeviceType value) = value

    let parse: Parser<Field<DeviceTypeRaw>> =
        parseField "Device Type" parseU8 |>> Field.map RawDeviceType

module DeviceType =

    let value (DeviceType value) = value

    /// Creates a concrete device-type code, excluding the FF wildcard (EN 13757-7, 7.5.4).
    let create value =
        if value < 0 || value > 254 then
            Error "Device type must be between 0 and 254; 255 is a selection wildcard"
        else
            Ok(DeviceType(byte value))

    let validate (raw: Field<DeviceTypeRaw>) : Validation<Field<DeviceType>> =

        let value = DeviceTypeRaw.value raw.Value

        if value = 0xFFuy then
            failed raw "Wildcard byte 0xFF is not allowed in device type identification"
        else
            raw |> Field.withValue (DeviceType value) |> passed

    /// Writes the validated DeviceType field.
    let write (field: DeviceType) : Writer<unit> = writeU8 (value field)
