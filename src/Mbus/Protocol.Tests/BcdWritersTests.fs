module Metering.Mbus.Protocol.Tests.BcdWritersTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Utility.BcdWriters
open Metering.Common.BaseWriters.Core

let writerAtPos pos testWriter =
    writer {
        do! seek pos
        do! testWriter
    }

let runWriterOk st0 pos writer =
    match writerAtPos pos writer st0 with
    | Ok(_, st1) -> st1
    | Error e -> failwithf $"Unexpected: %A{e}"

let runWriterError st0 pos writer =
    match writerAtPos pos writer st0 with
    | Error e -> e
    | Ok _ -> failwith "Expected failure"

[<Fact>]
let ``writeBcdU8 at first position returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU8 12uy
    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 1
    (WState.buf resultState)[0] |> should equal 0x12uy

[<Fact>]
let ``writeBcdU8 at offset returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU8 99uy
    let resultState = runWriterOk testState 10 writer
    WState.pos resultState |> should equal 11
    (WState.buf resultState)[10] |> should equal 0x99uy

[<Fact>]
let ``chained writeBcdU8 returns expected state`` () =
    let testState = WState.create ()

    let writer =
        writer {
            do! writeBcdU8 56uy
            do! writeBcdU8 78uy
        }

    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0x56uy; 0x78uy |]

[<Fact>]
let ``writeBcdU8 overflow returns error`` () =
    let testState = WState.create ()
    let writer = writeBcdU8 12uy
    let error = runWriterError testState 256 writer
    error.Pos |> should equal 256
    error.Msg |> should equal "not enough space in buffer to write 1 bytes"

[<Fact>]
let ``writeBcdU16LittleEndian at first position returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU16LittleEndian 1234us
    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 2
    (WState.buf resultState)[0..1] |> should equal [| 0x34uy; 0x12uy |]

[<Fact>]
let ``writeBcdU16LittleEndian at offset returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU16LittleEndian 9876us
    let resultState = runWriterOk testState 10 writer
    WState.pos resultState |> should equal 12
    (WState.buf resultState)[10..11] |> should equal [| 0x76uy; 0x98uy |]

[<Fact>]
let ``chained writeBcdU16LittleEndian returns expected state`` () =
    let testState = WState.create ()

    let writer =
        writer {
            do! writeBcdU16LittleEndian 1122us
            do! writeBcdU16LittleEndian 3344us
        }

    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 4

    (WState.buf resultState)[0..3]
    |> should equal [| 0x22uy; 0x11uy; 0x44uy; 0x33uy |]

[<Fact>]
let ``writeBcdU16LittleEndian overflow returns error`` () =
    let testState = WState.create ()
    let writer = writeBcdU16LittleEndian 1234us
    let error = runWriterError testState 255 writer
    error.Pos |> should equal 255
    error.Msg |> should equal "not enough space in buffer to write 2 bytes"

[<Fact>]
let ``writeBcdU24LittleEndian at first position returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU24LittleEndian 123456u
    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 3
    (WState.buf resultState)[0..2] |> should equal [| 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``writeBcdU24LittleEndian at offset returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU24LittleEndian 654321u
    let resultState = runWriterOk testState 10 writer
    WState.pos resultState |> should equal 13
    (WState.buf resultState)[10..12] |> should equal [| 0x21uy; 0x43uy; 0x65uy |]

[<Fact>]
let ``chained writeBcdU24LittleEndian returns expected state`` () =
    let testState = WState.create ()

    let writer =
        writer {
            do! writeBcdU24LittleEndian 112233u
            do! writeBcdU24LittleEndian 445566u
        }

    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 6

    (WState.buf resultState)[0..5]
    |> should equal [| 0x33uy; 0x22uy; 0x11uy; 0x66uy; 0x55uy; 0x44uy |]

[<Fact>]
let ``writeBcdU24LittleEndian overflow returns error`` () =
    let testState = WState.create ()
    let writer = writeBcdU24LittleEndian 123456u
    let error = runWriterError testState 254 writer
    error.Pos |> should equal 254
    error.Msg |> should equal "not enough space in buffer to write 3 bytes"

[<Fact>]
let ``writeBcdU32LittleEndian at first position returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU32LittleEndian 12345678u
    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 4

    (WState.buf resultState)[0..3]
    |> should equal [| 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``writeBcdU32LittleEndian at offset returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU32LittleEndian 87654321u
    let resultState = runWriterOk testState 10 writer
    WState.pos resultState |> should equal 14

    (WState.buf resultState)[10..13]
    |> should equal [| 0x21uy; 0x43uy; 0x65uy; 0x87uy |]

[<Fact>]
let ``chained writeBcdU32LittleEndian returns expected state`` () =
    let testState = WState.create ()

    let writer =
        writer {
            do! writeBcdU32LittleEndian 11223344u
            do! writeBcdU32LittleEndian 55667788u
        }

    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 8

    (WState.buf resultState)[0..7]
    |> should equal [| 0x44uy; 0x33uy; 0x22uy; 0x11uy; 0x88uy; 0x77uy; 0x66uy; 0x55uy |]

[<Fact>]
let ``writeBcdU32LittleEndian overflow returns error`` () =
    let testState = WState.create ()
    let writer = writeBcdU32LittleEndian 12345678u
    let error = runWriterError testState 253 writer
    error.Pos |> should equal 253
    error.Msg |> should equal "not enough space in buffer to write 4 bytes"

[<Fact>]
let ``writeBcdU48LittleEndian at first position returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU48LittleEndian 123456789012UL
    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 6

    (WState.buf resultState)[0..5]
    |> should equal [| 0x12uy; 0x90uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

[<Fact>]
let ``writeBcdU48LittleEndian at offset returns expected state`` () =
    let testState = WState.create ()
    let writer = writeBcdU48LittleEndian 987654321098UL
    let resultState = runWriterOk testState 10 writer
    WState.pos resultState |> should equal 16

    (WState.buf resultState)[10..15]
    |> should equal [| 0x98uy; 0x10uy; 0x32uy; 0x54uy; 0x76uy; 0x98uy |]

[<Fact>]
let ``chained writeBcdU48LittleEndian returns expected state`` () =
    let testState = WState.create ()

    let writer =
        writer {
            do! writeBcdU48LittleEndian 112233445566UL
            do! writeBcdU48LittleEndian 778899001122UL
        }

    let resultState = runWriterOk testState 0 writer

    WState.pos resultState |> should equal 12

    (WState.buf resultState)[0..11]
    |> should equal [|
        0x66uy
        0x55uy
        0x44uy
        0x33uy
        0x22uy
        0x11uy
        0x22uy
        0x11uy
        0x00uy
        0x99uy
        0x88uy
        0x77uy
    |]

[<Fact>]
let ``writeBcdU48LittleEndian overflow returns error`` () =
    let testState = WState.create ()
    let writer = writeBcdU48LittleEndian 123456789012UL
    let error = runWriterError testState 251 writer
    error.Pos |> should equal 251
    error.Msg |> should equal "not enough space in buffer to write 6 bytes"
