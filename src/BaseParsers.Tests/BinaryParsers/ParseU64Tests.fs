module Mbus.BaseParsers.Tests.BinaryParsers.ParseU64Tests

open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Tests
open Xunit

let runPU64 = TestHelpers.runParser parseU64LittleEndian
let runPU64Err = TestHelpers.runFailingParser parseU64LittleEndian

[<Fact>]
let ``parseU64 at first position returns first u64 and advances by eight`` () =
    let testState =
        TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy; 0x88uy; 0x99uy |] 0

    let expected = 0x8877665544332211UL
    runPU64 testState expected 8

[<Fact>]
let ``parseU64 at second position returns u64 at second position and advances by eight`` () =
    let testState =
        TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy; 0x88uy; 0x99uy |] 1

    let expected = 0x9988776655443322UL
    runPU64 testState expected 9

[<Fact>]
let ``parseU64 at eighth last position returns last u64 and advances by eight`` () =
    let testState =
        TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy; 0x88uy |] 0

    let expected = 0x8877665544332211UL
    runPU64 testState expected 8

[<Fact>]
let ``parseU64 max value returns 0xFFFFFFFFFFFFFFFF and advances by eight`` () =
    let testState = TestStateHelpers.create [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |] 0
    let expected = 0xFFFFFFFFFFFFFFFFUL
    runPU64 testState expected 8

[<Fact>]
let ``parseU64 fails on empty buffer`` () =
    runPU64Err TestStateHelpers.empty "unexpected end of buffer" 0

[<Fact>]
let ``parseU64 fails at last position`` () =
    let testState = TestStateHelpers.create [| 0x11uy |] 0
    runPU64Err testState "unexpected end of buffer" 1

[<Fact>]
let ``parseU64 fails at seventh last position`` () =
    let testState = TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy |] 0
    runPU64Err testState "unexpected end of buffer" 7
