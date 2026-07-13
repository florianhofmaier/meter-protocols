module Mbus.BaseWriters.Tests.BinaryWritersTests

open Xunit
open FsUnit.Xunit
open Mbus.BaseWriters.BinaryWriters
open Mbus.BaseWriters.Core

let writerAtPos pos testWriter =
    writer {
        do! seek pos
        do! testWriter
    }

let runWriterOk st0 pos w =
    match writerAtPos pos w st0 with
    | Ok (_, st1) -> st1
    | Error e -> failwithf $"Unexpected: %A{e}"

let runWriterError st0 pos w =
    match writerAtPos pos w st0 with
    | Error e -> e
    | Ok _ -> failwith "Expected failure"

[<Fact>]
let ``writeU8 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU8 0x42uy
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 1
    (WState.buf resultState)[0] |> should equal 0x42uy

[<Fact>]
let ``writeU8 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU8 0xAAuy
    let resultState = runWriterOk testState 14 w
    WState.pos resultState |> should equal 15
    (WState.buf resultState)[14] |> should equal 0xAAuy

[<Fact>]
let ``chained writeU8 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU8 0x01uy
            do! writeU8 0x02uy
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0x01uy; 0x02uy |]

[<Fact>]
let ``writeU8 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU8 0x42uy
    let error = runWriterError testState 256 w
    error.Pos |> should equal 256
    error.Msg |> should equal "not enough space in buffer to write 1 bytes"

[<Fact>]
let ``writeI8 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI8 -1y
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 1
    (WState.buf resultState)[0] |> should equal 0xFFuy

[<Fact>]
let ``writeI8 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI8 -128y
    let resultState = runWriterOk testState 14 w
    WState.pos resultState |> should equal 15
    (WState.buf resultState)[14] |> should equal 0x80uy

[<Fact>]
let ``chained writeI8 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI8 -1y     // 0xFF
            do! writeI8 1y      // 0x01
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0xFFuy; 0x01uy |]

[<Fact>]
let ``writeI8 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI8 -1y
    let error = runWriterError testState 256 w
    error.Pos |> should equal 256
    error.Msg |> should equal "not enough space in buffer to write 1 bytes"

[<Fact>]
let ``writeU16 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU16 0xAABBus
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0xBBuy; 0xAAuy |] // Little Endian

[<Fact>]
let ``writeU16 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU16 0x1234us
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 12
    (WState.buf resultState)[10..11] |> should equal [| 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeU16 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU16 0x1122us
            do! writeU16 0x3344us
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 4
    (WState.buf resultState)[0..3] |> should equal [| 0x22uy; 0x11uy; 0x44uy; 0x33uy |]

[<Fact>]
let ``writeU16 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU16 0xAABBus
    let error = runWriterError testState 255 w
    error.Pos |> should equal 255
    error.Msg |> should equal "not enough space in buffer to write 2 bytes"

[<Fact>]
let ``writeI16 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI16 -1s
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI16 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI16 -32768s
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 12
    (WState.buf resultState)[10..11] |> should equal [| 0x00uy; 0x80uy |]

[<Fact>]
let ``chained writeI16 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI16 -1s
            do! writeI16 258s   // 0x0102
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 4
    (WState.buf resultState)[0..3] |> should equal [| 0xFFuy; 0xFFuy; 0x02uy; 0x01uy |]

[<Fact>]
let ``writeI16 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI16 -1s
    let error = runWriterError testState 255 w
    error.Pos |> should equal 255
    error.Msg |> should equal "not enough space in buffer to write 2 bytes"

[<Fact>]
let ``writeU24 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU24 0xAABBCCu
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 3
    (WState.buf resultState)[0..2] |> should equal [| 0xCCuy; 0xBBuy; 0xAAuy |]

[<Fact>]
let ``writeU24 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU24 0x112233u
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 13
    (WState.buf resultState)[10..12] |> should equal [| 0x33uy; 0x22uy; 0x11uy |]

[<Fact>]
let ``chained writeU24 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU24 0x112233u
            do! writeU24 0x445566u
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 6
    (WState.buf resultState)[0..5] |> should equal [| 0x33uy; 0x22uy; 0x11uy; 0x66uy; 0x55uy; 0x44uy |]

[<Fact>]
let ``writeU24 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU24 0xAABBCCu
    let error = runWriterError testState 254 w
    error.Pos |> should equal 254
    error.Msg |> should equal "not enough space in buffer to write 3 bytes"

[<Fact>]
let ``writeI24 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI24 -1
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 3
    (WState.buf resultState)[0..2] |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI24 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI24 0x123456
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 13
    (WState.buf resultState)[10..12] |> should equal [| 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeI24 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI24 0x112233
            do! writeI24 -2         // 0xFFFFFE
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 6
    (WState.buf resultState)[0..5] |> should equal [| 0x33uy; 0x22uy; 0x11uy; 0xFEuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI24 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI24 -1
    let error = runWriterError testState 254 w
    error.Pos |> should equal 254
    error.Msg |> should equal "not enough space in buffer to write 3 bytes"

[<Fact>]
let ``writeU32 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU32 0xAABBCCDDu
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 4
    (WState.buf resultState)[0..3] |> should equal [| 0xDDuy; 0xCCuy; 0xBBuy; 0xAAuy |]

[<Fact>]
let ``writeU32 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU32 0x12345678u
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 14
    (WState.buf resultState)[10..13] |> should equal [| 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeU32 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU32 0x11223344u
            do! writeU32 0x55667788u
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 8
    (WState.buf resultState)[0..7] |> should equal [| 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0x88uy; 0x77uy; 0x66uy; 0x55uy |]

[<Fact>]
let ``writeU32 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU32 0xAABBCCDDu
    let error = runWriterError testState 253 w
    error.Pos |> should equal 253
    error.Msg |> should equal "not enough space in buffer to write 4 bytes"

[<Fact>]
let ``writeI32 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI32 -1
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 4
    (WState.buf resultState)[0..3] |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI32 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI32 0x12345678
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 14
    (WState.buf resultState)[10..13] |> should equal [| 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeI32 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI32 0x11223344
            do! writeI32 -2
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 8
    (WState.buf resultState)[0..7] |> should equal [| 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0xFEuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI32 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI32 -1
    let error = runWriterError testState 253 w
    error.Pos |> should equal 253
    error.Msg |> should equal "not enough space in buffer to write 4 bytes"

[<Fact>]
let ``writeU48 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU48 0xAABBCCDDEEFFUL
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 6
    (WState.buf resultState)[0..5] |> should equal [| 0xFFuy; 0xEEuy; 0xDDuy; 0xCCuy; 0xBBuy; 0xAAuy |]

[<Fact>]
let ``writeU48 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU48 0x112233445566UL
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 16
    (WState.buf resultState)[10..15] |> should equal [| 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy |]

[<Fact>]
let ``chained writeU48 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU48 0x112233445566UL
            do! writeU48 0x77889900AABBUL
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 12
    (WState.buf resultState)[0..11] |> should equal [| 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0xBBuy; 0xAAuy; 0x00uy; 0x99uy; 0x88uy; 0x77uy |]

[<Fact>]
let ``writeU48 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU48 0xAABBCCDDEEFFUL
    let error = runWriterError testState 251 w
    error.Pos |> should equal 251
    error.Msg |> should equal "not enough space in buffer to write 6 bytes"

[<Fact>]
let ``writeI48 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI48 -1L
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 6
    (WState.buf resultState)[0..5] |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI48 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI48 0x1234567890ABL
    let resultState = runWriterOk testState 10 w

    WState.pos resultState |> should equal 16
    (WState.buf resultState)[10..15] |> should equal [| 0xABuy; 0x90uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeI48 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI48 0x112233445566L
            do! writeI48 -2L
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 12
    (WState.buf resultState)[0..11] |> should equal [| 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0xFEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI48 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI48 -1L
    let error = runWriterError testState 251 w
    error.Pos |> should equal 251
    error.Msg |> should equal "not enough space in buffer to write 6 bytes"

[<Fact>]
let ``writeU64 at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeU64 0xAABBCCDDEEFF1122UL
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 8
    (WState.buf resultState)[0..7] |> should equal [| 0x22uy; 0x11uy; 0xFFuy; 0xEEuy; 0xDDuy; 0xCCuy; 0xBBuy; 0xAAuy |]

[<Fact>]
let ``writeU64 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeU64 0x1122334455667788UL
    let resultState = runWriterOk testState 10 w
    WState.pos resultState |> should equal 18
    (WState.buf resultState)[10..17] |> should equal [| 0x88uy; 0x77uy; 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy |]

[<Fact>]
let ``chained writeU64 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeU64 0x1122334455667788UL
            do! writeU64 0x99AABBCCDDEEFF00UL
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 16
    (WState.buf resultState)[0..15] |> should equal [| 0x88uy; 0x77uy; 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0x00uy; 0xFFuy; 0xEEuy; 0xDDuy; 0xCCuy; 0xBBuy; 0xAAuy; 0x99uy |]

[<Fact>]
let ``writeU64 overflow returns error`` () =
    let testState = WState.create()
    let w = writeU64 0xAABBCCDDEEFF1122UL
    let error = runWriterError testState 249 w
    error.Pos |> should equal 249
    error.Msg |> should equal "not enough space in buffer to write 8 bytes"

[<Fact>]
let ``writeI64 (negative) at first position returns expected state`` () =
    let testState = WState.create()
    let w = writeI64 -1L
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 8
    (WState.buf resultState)[0..7] |> should equal [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI64 at offset returns expected state`` () =
    let testState = WState.create()
    let w = writeI64 0x1234567890ABCDEFL
    let resultState = runWriterOk testState 10 w

    WState.pos resultState |> should equal 18
    (WState.buf resultState)[10..17] |> should equal [| 0xEFuy; 0xCDuy; 0xABuy; 0x90uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``chained writeI64 returns expected state`` () =
    let testState = WState.create()
    let w =
        writer {
            do! writeI64 0x1122334455667788L
            do! writeI64 -2L
        }
    let resultState = runWriterOk testState 0 w

    WState.pos resultState |> should equal 16
    (WState.buf resultState)[0..15] |> should equal [| 0x88uy; 0x77uy; 0x66uy; 0x55uy; 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0xFEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

[<Fact>]
let ``writeI64 overflow returns error`` () =
    let testState = WState.create()
    let w = writeI64 -1L
    let error = runWriterError testState 249 w
    error.Pos |> should equal 249
    error.Msg |> should equal "not enough space in buffer to write 8 bytes"
