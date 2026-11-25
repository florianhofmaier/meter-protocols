module Metering.Common.BaseWriters.Tests.OutputWriterTests

open System
open System.Text
open Xunit
open FsUnit.Xunit
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.BaseWriters.TextWriters

let private bytes =
    function
    | Ok value -> value
    | Error error -> failwith $"Unexpected writer error: {error}"

[<Fact>]
let ``memory writer copies only the requested source slice at the current offset`` () =
    let source = [| 99uy; 1uy; 2uy; 88uy |]

    let write =
        writer {
            do! writeU8 7uy
            do! writeMemory (ReadOnlyMemory(source, 1, 2))
            do! writeU8 8uy
        }

    Writer.run 4 write |> bytes |> should equal [| 7uy; 1uy; 2uy; 8uy |]

[<Fact>]
let ``memory writer supports overlapping source and target`` () =
    let buffer = [| 1uy; 2uy; 3uy; 4uy; 5uy |]

    match writeMemory (ReadOnlyMemory(buffer, 0, 4)) { Buf = buffer; Pos = 1 } with
    | Ok((), state) -> state.Pos |> should equal 5
    | Error error -> failwith $"Unexpected error {error}"

    buffer |> should equal [| 1uy; 1uy; 2uy; 3uy; 4uy |]

[<Fact>]
let ``memory capacity failure leaves all destination bytes unchanged`` () =
    let buffer = Array.create 4 0xCCuy

    match writeMemory (ReadOnlyMemory [| 1uy; 2uy; 3uy |]) { Buf = buffer; Pos = 2 } with
    | Error error -> error.Pos |> should equal 2
    | Ok _ -> failwith "Expected capacity error"

    buffer |> should equal (Array.create 4 0xCCuy)

[<Fact>]
let ``empty memory is accepted at the buffer end`` () =
    let state = { Buf = [||]; Pos = 0 }

    match writeMemory ReadOnlyMemory<byte>.Empty state with
    | Ok((), final) -> final.Pos |> should equal 0
    | Error error -> failwith $"Unexpected error {error}"

[<Theory>]
[<InlineData(0u)>]
[<InlineData(2147483648u)>]
[<InlineData(1065353216u)>]
[<InlineData(3217031168u)>]
[<InlineData(2139095040u)>]
[<InlineData(4286578688u)>]
[<InlineData(2143363909u)>]
let ``float writer preserves IEEE bits in little endian order`` (bits: uint32) =
    let value = BitConverter.UInt32BitsToSingle bits
    let expected = [| for shift in [ 0; 8; 16; 24 ] -> byte ((bits >>> shift) &&& 255u) |]
    Writer.run 4 (writeF32LittleEndian value) |> bytes |> should equal expected

[<Fact>]
let ``text writer uses the explicit encoding without preamble prefix or terminator`` () =
    Writer.run 8 (writeText Encoding.UTF8 "ä€")
    |> bytes
    |> should equal [| 0xC3uy; 0xA4uy; 0xE2uy; 0x82uy; 0xACuy |]

    Writer.run 8 (writeText Encoding.BigEndianUnicode "Aä")
    |> bytes
    |> should equal [| 0uy; 0x41uy; 0uy; 0xE4uy |]

[<Fact>]
let ``text fallback failure returns an error without changing the buffer`` () =
    let encoding =
        Encoding.GetEncoding(
            "us-ascii",
            EncoderFallback.ExceptionFallback,
            DecoderFallback.ExceptionFallback
        )

    let buffer = Array.create 4 0xCCuy

    match writeText encoding "ä" { Buf = buffer; Pos = 1 } with
    | Error error ->
        error.Pos |> should equal 1
        error.Ctx |> should equal [ "writeText" ]
    | Ok _ -> failwith "Expected encoding error"

    buffer |> should equal (Array.create 4 0xCCuy)

[<Fact>]
let ``text capacity is checked in encoded bytes without partial output`` () =
    let buffer = Array.create 4 0xCCuy

    match writeText Encoding.UTF8 "ä€" { Buf = buffer; Pos = 0 } with
    | Error error -> error.Pos |> should equal 0
    | Ok _ -> failwith "Expected capacity error"

    buffer |> should equal (Array.create 4 0xCCuy)

    Writer.run 0 (writeText Encoding.UTF8 "")
    |> bytes
    |> Array.isEmpty
    |> should be True
