module Metering.Mbus.Protocol.Tests.VariableLengthValueTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Tests.TestSupport

[<Fact>]
let ``positive BCD LVAR consumes declared payload bytes and leaves following marker`` () =
    let r = reader [| 0xC2uy; 0x34uy; 0x12uy; 0x99uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok (ValueRaw.PosBcd payload) ->
        payload.Value.ToArray() |> should equal [| 0x34uy; 0x12uy |]
        r.Position |> should equal 3
        r.Remaining |> should equal 1
        r.Peek(1).Span[0] |> should equal 0x99uy

    | Ok value ->
        failwith $"Expected positive BCD, got %A{value}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``negative BCD LVAR consumes declared payload bytes and leaves following marker`` () =
    let r = reader [| 0xD2uy; 0x34uy; 0x12uy; 0x99uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok (ValueRaw.NegBcd payload) ->
        payload.Value.ToArray() |> should equal [| 0x34uy; 0x12uy |]
        r.Position |> should equal 3
        r.Remaining |> should equal 1
        r.Peek(1).Span[0] |> should equal 0x99uy

    | Ok value ->
        failwith $"Expected negative BCD, got %A{value}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``variable BCD LVAR truncated payload returns structured parser error`` () =
    let r = reader [| 0xC3uy; 0x34uy; 0x12uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Error error ->
        error.Msg.Contains("Unexpected end of buffer") |> should be True

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``unsupported data field returns structured parser error`` () =
    let r = reader [||]

    match run r trace (ValueRaw.parse 0x0Fuy) with
    | Error error ->
        error.Msg.Contains("invalid data field") |> should be True

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

