module Metering.Common.Decoding.Parsers.Tests.BinaryParserTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types

let private parseExactly parser bytes =
    let r =
        reader bytes

    match runExactly r trace parser with
    | Ok value ->
        r.Position |> should equal bytes.Length
        r.Remaining |> should equal 0
        value

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

let private expectTruncated name parser bytes =
    match runExactly (reader bytes) trace parser with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Msg.Contains("Unexpected end of buffer") |> should equal true

    | Ok value ->
        failwith $"Expected truncation from {name}, got %A{value}"

[<Fact>]
let ``parseU8 reads one byte including zero and max`` () =
    parseExactly parseU8 [| 0x00uy |] |> should equal 0x00uy
    parseExactly parseU8 [| 0xFFuy |] |> should equal 0xFFuy

[<Fact>]
let ``peekU8 returns byte without consuming it`` () =
    let r =
        reader [| 0xAAuy |]

    match run r trace peekU8 with
    | Ok value ->
        value |> should equal 0xAAuy
        r.Position |> should equal 0
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``parseI8 reads signed two complement values`` () =
    parseExactly parseI8 [| 0x00uy |] |> should equal 0y
    parseExactly parseI8 [| 0x7Fuy |] |> should equal 127y
    parseExactly parseI8 [| 0xFFuy |] |> should equal -1y
    parseExactly parseI8 [| 0x80uy |] |> should equal -128y

[<Fact>]
let ``parseU16LittleEndian reads asymmetric bytes`` () =
    parseExactly parseU16LittleEndian [| 0x01uy; 0x02uy |]
    |> should equal 0x0201us

    parseExactly parseU16LittleEndian [| 0x00uy; 0x00uy |]
    |> should equal 0x0000us

[<Fact>]
let ``parseU16BigEndian reads asymmetric bytes`` () =
    parseExactly parseU16BigEndian [| 0x01uy; 0x02uy |]
    |> should equal 0x0102us

[<Fact>]
let ``parseI16LittleEndian reads signed values`` () =
    parseExactly parseI16LittleEndian [| 0xFFuy; 0x7Fuy |] |> should equal 32767s
    parseExactly parseI16LittleEndian [| 0xFFuy; 0xFFuy |] |> should equal -1s
    parseExactly parseI16LittleEndian [| 0x00uy; 0x80uy |] |> should equal -32768s

[<Fact>]
let ``parseI16BigEndian reads signed values`` () =
    parseExactly parseI16BigEndian [| 0x7Fuy; 0xFFuy |] |> should equal 32767s
    parseExactly parseI16BigEndian [| 0xFFuy; 0xFFuy |] |> should equal -1s
    parseExactly parseI16BigEndian [| 0x80uy; 0x00uy |] |> should equal -32768s

[<Fact>]
let ``parseU24LittleEndian treats first byte as least significant`` () =
    parseExactly parseU24LittleEndian [| 0x01uy; 0x02uy; 0x03uy |]
    |> should equal 0x030201u

    parseExactly parseU24LittleEndian [| 0x00uy; 0x00uy; 0x00uy |]
    |> should equal 0u

[<Fact>]
let ``parseI24LittleEndian sign-extends two complement values`` () =
    parseExactly parseI24LittleEndian [| 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> should equal 8_388_607

    parseExactly parseI24LittleEndian [| 0xFEuy; 0xFFuy; 0xFFuy |]
    |> should equal -2

    parseExactly parseI24LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal -1

    parseExactly parseI24LittleEndian [| 0x00uy; 0x00uy; 0x80uy |]
    |> should equal -8_388_608

[<Fact>]
let ``parseU32LittleEndian and big endian read asymmetric bytes`` () =
    parseExactly parseU32LittleEndian [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
    |> should equal 0x04030201u

    parseExactly parseU32BigEndian [| 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
    |> should equal 0x01020304u

    parseExactly parseU32LittleEndian [| 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
    |> should equal 0u

[<Fact>]
let ``parseI32LittleEndian and big endian read signed values`` () =
    parseExactly parseI32LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> should equal Int32.MaxValue

    parseExactly parseI32BigEndian [| 0x7Fuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal Int32.MaxValue

    parseExactly parseI32LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal -1

    parseExactly parseI32BigEndian [| 0x80uy; 0x00uy; 0x00uy; 0x00uy |]
    |> should equal Int32.MinValue

[<Fact>]
let ``parseU48LittleEndian treats first byte as least significant`` () =
    parseExactly parseU48LittleEndian [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy |]
    |> should equal 0x060504030201UL

    parseExactly parseU48LittleEndian [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
    |> should equal 0UL

[<Fact>]
let ``parseI48LittleEndian sign-extends two complement values`` () =
    parseExactly parseI48LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> should equal 140_737_488_355_327L

    parseExactly parseI48LittleEndian [| 0xFEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal -2L

    parseExactly parseI48LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal -1L

    parseExactly parseI48LittleEndian [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]
    |> should equal -140_737_488_355_328L

[<Fact>]
let ``parseU64LittleEndian reads full unsigned width`` () =
    parseExactly parseU64LittleEndian [| 0x11uy; 0x22uy; 0x33uy; 0x44uy; 0x55uy; 0x66uy; 0x77uy; 0x88uy |]
    |> should equal 0x8877665544332211UL

    parseExactly parseU64LittleEndian [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |]
    |> should equal 0UL

[<Fact>]
let ``parseI64LittleEndian reads signed values`` () =
    parseExactly parseI64LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> should equal Int64.MaxValue

    parseExactly parseI64LittleEndian [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> should equal -1L

    parseExactly parseI64LittleEndian [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]
    |> should equal Int64.MinValue

[<Fact>]
let ``expectU8 succeeds and reports mismatch after consumption`` () =
    parseExactly (expectU8 0xAAuy) [| 0xAAuy |]
    |> should equal ()

    let r =
        reader [| 0xBBuy |]

    match run r trace (expectU8 0xAAuy) with
    | Error error ->
        error.Pos |> should equal 1
        error.Msg |> should equal "expected 170, but got 187"
        r.Position |> should equal 1

    | Ok () ->
        failwith "Expected parser error"

[<Fact>]
let ``expectU16BigEndian succeeds and reports mismatch after consumption`` () =
    parseExactly (expectU16BigEndian 0xAABBus) [| 0xAAuy; 0xBBuy |]
    |> should equal ()

    let r =
        reader [| 0xAAuy; 0xBCuy |]

    match run r trace (expectU16BigEndian 0xAABBus) with
    | Error error ->
        error.Pos |> should equal 2
        error.Msg |> should equal "expected 43707, but got 43708"
        r.Position |> should equal 2

    | Ok () ->
        failwith "Expected parser error"

[<Fact>]
let ``all binary parsers return structured truncation errors`` () =
    [
        "parseU8", parseU8 |>> box, [||]
        "peekU8", peekU8 |>> box, [||]
        "parseI8", parseI8 |>> box, [||]
        "parseU16LittleEndian", parseU16LittleEndian |>> box, [| 0x01uy |]
        "parseU16BigEndian", parseU16BigEndian |>> box, [| 0x01uy |]
        "parseI16LittleEndian", parseI16LittleEndian |>> box, [| 0x01uy |]
        "parseI16BigEndian", parseI16BigEndian |>> box, [| 0x01uy |]
        "parseU24LittleEndian", parseU24LittleEndian |>> box, [| 0x01uy; 0x02uy |]
        "parseI24LittleEndian", parseI24LittleEndian |>> box, [| 0x01uy; 0x02uy |]
        "parseU32LittleEndian", parseU32LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy |]
        "parseU32BigEndian", parseU32BigEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy |]
        "parseI32LittleEndian", parseI32LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy |]
        "parseI32BigEndian", parseI32BigEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy |]
        "parseU48LittleEndian", parseU48LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
        "parseI48LittleEndian", parseI48LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
        "parseU64LittleEndian", parseU64LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy |]
        "parseI64LittleEndian", parseI64LittleEndian |>> box, [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy; 0x07uy |]
    ]
    |> List.iter (fun (name, parser, bytes) -> expectTruncated name parser bytes)
