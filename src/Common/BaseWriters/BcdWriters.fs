module Mbus.BaseWriters.BcdWriters

open Mbus.BaseWriters.Core

let inline private bcdByte (d2: uint32) : byte =
    let tens = d2 / 10u
    let ones = d2 % 10u
    byte ((tens <<< 4) ||| ones)

let inline private writeBcdBytes (bcdByteCount: int) (value: uint64) : Writer<unit> =
    checkedWrite bcdByteCount (fun st ->
        let mutable v = value
        for i = 0 to bcdByteCount - 1 do
            let d2 = uint32 (v % 100UL)
            st.Buf[st.Pos + i] <- bcdByte d2
            v <- v / 100UL)

let writeBcdU8 (value: uint8) : Writer<unit> =
    writeBcdBytes 1 (uint64 value)

let writeBcdU16 (value: uint16) : Writer<unit> =
    writeBcdBytes 2 (uint64 value)

let writeBcdU24 (value: uint32) : Writer<unit> =
    writeBcdBytes 3 (uint64 value)

let writeBcdU32 (value: uint32) : Writer<unit> =
    writeBcdBytes 4 (uint64 value)

let writeBcdU48 (value: uint64) : Writer<unit> =
    writeBcdBytes 6 value