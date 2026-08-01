namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Utility

type IdNumberRaw =
    private RawIdNumber of ReadOnlyMemory<byte>

type IdNumber =
    private
        IdNumber of uint32

module IdNumberRaw =

    let private length = 4

    let bytes (RawIdNumber value) =
        value

    let parse : Parser<Field<IdNumberRaw>> =
        parseField
            "Identification Number"
            (take length |>> RawIdNumber)

    let copyTo destination raw =
        (bytes raw).Span.CopyTo destination

    let toUint32 raw =
        let mem = bytes raw
        BinaryPrimitives.ReadUInt32LittleEndian mem.Span

module IdNumber =

    let private digitCount = 8

    let value (IdNumber value) =
        value

    let toBcd (IdNumber value) =
        Bcd.encodeUInt32 digitCount value

    let fromRaw
        (raw: Field<IdNumberRaw>)
        : Validation<Field<IdNumber>> =

        let value = IdNumberRaw.toUint32 raw.Value

        match Bcd.tryFindInvalidBcdNibble digitCount value with
        | Some (_, nibble) ->
            failed raw $"Invalid BCD nibble 0x{nibble:X} in identification number"

        | None ->
            value
            |> Bcd.decodeUInt32 digitCount
            |> IdNumber
            |> fun value -> Field.withValue value raw
            |> passed

