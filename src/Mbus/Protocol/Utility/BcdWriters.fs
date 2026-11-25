module Metering.Mbus.Protocol.Utility.BcdWriters

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

let inline private bcdByte (d2: uint32) : byte =
    let tens = d2 / 10u
    let ones = d2 % 10u
    byte ((tens <<< 4) ||| ones)

let inline private writeBcdBytesLittleEndian (bcdByteCount: int) (value: uint64) : Writer<unit> =
    let bytes = Array.zeroCreate<byte> bcdByteCount
    let mutable v = value

    for i = 0 to bcdByteCount - 1 do
        let d2 = uint32 (v % 100UL)
        bytes[i] <- bcdByte d2
        v <- v / 100UL

    writeMemory (ReadOnlyMemory bytes)

/// <summary>Writes the lowest 2 decimal digits as 1 packed BCD byte, least significant pair first; discards higher digits and returns an error if the buffer is too small.</summary>
let writeBcdU8 (value: uint8) : Writer<unit> =
    writeBcdBytesLittleEndian 1 (uint64 value)

/// <summary>Writes the lowest 4 decimal digits as 2 packed BCD bytes, least significant pair first; discards higher digits and returns an error if the buffer is too small.</summary>
let writeBcdU16LittleEndian (value: uint16) : Writer<unit> =
    writeBcdBytesLittleEndian 2 (uint64 value)

/// <summary>Writes the lowest 6 decimal digits as 3 packed BCD bytes, least significant pair first; discards higher digits and returns an error if the buffer is too small.</summary>
let writeBcdU24LittleEndian (value: uint32) : Writer<unit> =
    writeBcdBytesLittleEndian 3 (uint64 value)

/// <summary>Writes the lowest 8 decimal digits as 4 packed BCD bytes, least significant pair first; discards higher digits and returns an error if the buffer is too small.</summary>
let writeBcdU32LittleEndian (value: uint32) : Writer<unit> =
    writeBcdBytesLittleEndian 4 (uint64 value)

/// <summary>Writes the lowest 12 decimal digits as 6 packed BCD bytes, least significant pair first; discards higher digits and returns an error if the buffer is too small.</summary>
let writeBcdU48LittleEndian (value: uint64) : Writer<unit> = writeBcdBytesLittleEndian 6 value
