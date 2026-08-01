namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type ManufacturerRaw =
    private
        RawManufacturer of ReadOnlyMemory<byte>

type Manufacturer =
    private
        Manufacturer of uint16

module ManufacturerRaw =

    let private length = 2

    let bytes (RawManufacturer value) =
        value

    let parse : Parser<Field<ManufacturerRaw>> =
        parseField
            "Manufacturer"
            (take length |>> RawManufacturer)

    let copyTo destination raw =
        (bytes raw).Span.CopyTo destination

    let toUint16 raw =
        let mem = bytes raw
        BinaryPrimitives.ReadUInt16LittleEndian mem.Span

module private ManufacturerBytes =

    let low value =
        byte value

    let high value =
        byte (value >>> 8)

    let containsWildcard value =
        low value = 0xFFuy || high value = 0xFFuy

module Manufacturer =

    let value (Manufacturer value) =
        value

    let fromRaw
        (raw: Field<ManufacturerRaw>)
        : Validation<Field<Manufacturer>> =

        let value = ManufacturerRaw.toUint16 raw.Value

        if ManufacturerBytes.containsWildcard value then
            failed raw "Wildcard byte 0xFF is not allowed in manufacturer identification"
        else
            raw
            |> Field.withValue (Manufacturer value)
            |> passed

