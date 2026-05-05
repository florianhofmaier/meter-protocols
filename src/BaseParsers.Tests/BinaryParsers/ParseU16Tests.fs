module Mbus.BaseParsers.Tests.BinaryParsers.ParseU16Tests

open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Tests
open Xunit

let runPU16 = TestHelpers.runParser parseU16LittleEndian
let runPU16Err = TestHelpers.runFailingParser parseU16LittleEndian

[<Fact>]
let ``parseU16 at first position returns first u16 and advances by two`` () =
    let testState = TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy |] 0
    let expected = 0x2211us
    runPU16 testState expected 2

[<Fact>]
let ``parseU16 at second position returns u16 at second position and advances by two`` () =
    let testState = TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy |] 1
    let expected = 0x3322us
    runPU16 testState expected 3

[<Fact>]
let ``parseU16 at second last position returns last u16 and advances by two`` () =
    let testState = TestStateHelpers.create [| 0x11uy; 0x22uy; 0x33uy; 0x44uy |] 2
    let expected = 0x4433us
    runPU16 testState expected 4

[<Fact>]
let ``parseU16 max value returns 0xFFFF and advances by two`` () =
    let testState = TestStateHelpers.create [| 0xFFuy; 0xFFuy |] 0
    let expected = 0xFFFFus
    runPU16 testState expected 2

[<Fact>]
let ``parseU16 fails on empty buffer`` () =
    runPU16Err TestStateHelpers.empty "unexpected end of buffer" 0

[<Fact>]
let ``parseU16 fails at last position`` () =
    let testState = TestStateHelpers.create [| 0x11uy |] 0
    runPU16Err testState "unexpected end of buffer" 1
