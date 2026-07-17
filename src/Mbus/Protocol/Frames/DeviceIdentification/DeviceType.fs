namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type DeviceTypeRaw =
    private RawDeviceType of uint8

type DeviceType =
    private DeviceType of uint8

module DeviceTypeRaw =

    let value (RawDeviceType value) =
        value

    let parse : Parser<Field<DeviceTypeRaw>> =
        parseField "Device Type" parseU8
        |>> Field.map RawDeviceType

module DeviceType =

    let value (DeviceType value) =
        value

    let fromRaw
        (raw: Field<DeviceTypeRaw>)
        : Validation<Field<DeviceType>> =

        let value = DeviceTypeRaw.value raw.Value

        if value = 0xFFuy then
            failed raw "Wildcard byte 0xFF is not allowed in device type identification"
        else
            raw
            |> Field.withValue (DeviceType value)
            |> passed

