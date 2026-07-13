module Mbus.BaseWriters.BinaryWriters

open Mbus.BaseWriters.Core

let inline private u8bits (v: int8) : uint64 = uint64 (uint8 v)
let inline private u16bits (v: int16) : uint64 = uint64 (uint16 v)
let inline private u32bits (v: int32) : uint64 = uint64 (uint32 v)
let inline private u64bits (v: int64) : uint64 = uint64 v

let writeU8 (value: uint8) : Writer<unit> =
    writeBytes 1 (uint64 value)

let writeI8 (value: int8) : Writer<unit> =
    writeBytes 1 (u8bits value)

let writeU16 (value: uint16) : Writer<unit> =
    writeBytes 2 (uint64 value)

let writeI16 (value: int16) : Writer<unit> =
    writeBytes 2 (u16bits value)

let writeU24 (value: uint32) : Writer<unit> =
    writeBytes 3 (uint64 value)

let writeI24 (value: int32) : Writer<unit> =
    writeBytes 3 (u32bits value &&& 0xFFFFFFUL)

let writeU32 (value: uint32) : Writer<unit> =
    writeBytes 4 (uint64 value)

let writeI32 (value: int32) : Writer<unit> =
    writeBytes 4 (u32bits value)

let writeU48 (value: uint64) : Writer<unit> =
    writeBytes 6 value

let writeI48 (value: int64) : Writer<unit> =
    writeBytes 6 (u64bits value &&& 0xFFFFFFFFFFFFUL)

let writeU64 (value: uint64) : Writer<unit> =
    writeBytes 8 value

let writeI64 (value: int64) : Writer<unit> =
    writeBytes 8 (u64bits value)


