namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type DeviceTypeRaw =
    private DeviceType of uint8

module DeviceTypeRaw =

    let parse : Parser<ParsedField<DeviceTypeRaw>> =
        parseField "Device Type" parseU8
        |>> ParsedField.map DeviceType