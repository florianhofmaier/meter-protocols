module Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.BaseWriters.Core

let inline private u8bits (v: int8) : uint64 = uint64 (uint8 v)
let inline private u16bits (v: int16) : uint64 = uint64 (uint16 v)
let inline private u32bits (v: int32) : uint64 = uint64 (uint32 v)
let inline private u64bits (v: int64) : uint64 = uint64 v

/// <summary>Writes the unsigned 8-bit value, advancing on success or returning a buffer-capacity error.</summary>
let writeU8 (value: uint8) : Writer<unit> = writeBytesLittleEndian 1 (uint64 value)

/// <summary>Writes the signed 8-bit value, advancing on success or returning a buffer-capacity error.</summary>
let writeI8 (value: int8) : Writer<unit> = writeBytesLittleEndian 1 (u8bits value)

/// <summary>Writes the low 16 bits of the unsigned value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeU16LittleEndian (value: uint16) : Writer<unit> = writeBytesLittleEndian 2 (uint64 value)

/// <summary>Writes the low 16 bits of the signed value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeI16LittleEndian (value: int16) : Writer<unit> =
    writeBytesLittleEndian 2 (u16bits value)

/// <summary>Writes the low 24 bits of the unsigned value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeU24LittleEndian (value: uint32) : Writer<unit> = writeBytesLittleEndian 3 (uint64 value)

/// <summary>Writes the low 24 bits of the signed value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeI24LittleEndian (value: int32) : Writer<unit> =
    writeBytesLittleEndian 3 (u32bits value &&& 0xFFFFFFUL)

/// <summary>Writes the low 32 bits of the unsigned value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeU32LittleEndian (value: uint32) : Writer<unit> = writeBytesLittleEndian 4 (uint64 value)

/// <summary>Writes the low 32 bits of the signed value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeI32LittleEndian (value: int32) : Writer<unit> =
    writeBytesLittleEndian 4 (u32bits value)

/// <summary>Writes the low 48 bits of the unsigned value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeU48LittleEndian (value: uint64) : Writer<unit> = writeBytesLittleEndian 6 value

/// <summary>Writes the low 48 bits of the signed value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeI48LittleEndian (value: int64) : Writer<unit> =
    writeBytesLittleEndian 6 (u64bits value &&& 0xFFFFFFFFFFFFUL)

/// <summary>Writes the low 64 bits of the unsigned value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeU64LittleEndian (value: uint64) : Writer<unit> = writeBytesLittleEndian 8 value

/// <summary>Writes the low 64 bits of the signed value in little-endian order, advancing on success or returning a buffer-capacity error.</summary>
let writeI64LittleEndian (value: int64) : Writer<unit> =
    writeBytesLittleEndian 8 (u64bits value)

/// <summary>Copies the supplied byte range at the current position. A capacity error leaves the buffer unchanged; overlapping source and destination are supported.</summary>
let writeMemory (value: ReadOnlyMemory<byte>) : Writer<unit> =
    checkedWrite value.Length (fun state ->
        value.Span.CopyTo(state.Buf.AsSpan(state.Pos, value.Length)))

/// <summary>Writes the exact IEEE 754 binary32 representation in little-endian order, including signed zero, infinities and NaN payload bits.</summary>
let writeF32LittleEndian (value: float32) : Writer<unit> =
    writeU32LittleEndian (BitConverter.SingleToUInt32Bits value)
