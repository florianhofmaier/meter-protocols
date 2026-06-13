namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type ManufacturerRaw =
    private
        ManufacturerRaw of uint16

module ManufacturerRaw =

    let parse : Parser<ParsedField<ManufacturerRaw>> =
        parseField "Manufacturer" parseU16LittleEndian
        |>> ParsedField.map ManufacturerRaw