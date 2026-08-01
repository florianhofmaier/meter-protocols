module Metering.Mbus.Protocol.Tests.VariableLengthValueTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Tests.TestSupport

let private peekU8 reader =
    match reader.Peek 1 with
    | Ok bytes -> bytes.Span[0]
    | Error error -> failwith error.Msg

let private parsedRawField raw length =
    {
        Id = FieldId.create 99
        Span =
            {
                Source = SourceId.root
                Offset = 0
                Length = length
            }
        Value = raw
    }

let private parseAndValidate bytes =
    let r = reader bytes

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok raw ->
        parsedRawField raw r.Position
        |> Value.fromRaw

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``positive BCD LVAR consumes declared payload bytes and leaves following marker`` () =
    let r = reader [| 0xC2uy; 0x34uy; 0x12uy; 0x99uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok (ValueRaw.PosBcd payload) ->
        payload.Value.ToArray() |> should equal [| 0x34uy; 0x12uy |]
        r.Position |> should equal 3
        r.Remaining |> should equal 1
        peekU8 r |> should equal 0x99uy

    | Ok value ->
        failwith $"Expected positive BCD, got %A{value}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``positive BCD LVAR parses and validates to BCD value`` () =
    match parseAndValidate [| 0xC2uy; 0x34uy; 0x12uy |] with
    | Passed (Value.PosBcd value, _) ->
        value |> should equal 1234UL

    | actual ->
        failwith $"Expected positive BCD value, got %A{actual}"

[<Fact>]
let ``negative BCD LVAR consumes declared payload bytes and leaves following marker`` () =
    let r = reader [| 0xD2uy; 0x34uy; 0x12uy; 0x99uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok (ValueRaw.NegBcd payload) ->
        payload.Value.ToArray() |> should equal [| 0x34uy; 0x12uy |]
        r.Position |> should equal 3
        r.Remaining |> should equal 1
        peekU8 r |> should equal 0x99uy

    | Ok value ->
        failwith $"Expected negative BCD, got %A{value}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``negative BCD LVAR parses and validates to negative BCD value`` () =
    match parseAndValidate [| 0xD2uy; 0x34uy; 0x12uy |] with
    | Passed (Value.NegBcd value, _) ->
        value |> should equal -1234L

    | actual ->
        failwith $"Expected negative BCD value, got %A{actual}"

[<Fact>]
let ``variable BCD LVAR truncated payload returns structured parser error`` () =
    let r = reader [| 0xC3uy; 0x34uy; 0x12uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Error error ->
        error.Msg.Contains("Unexpected end of buffer") |> should be True

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``invalid variable BCD digits produce validation failure on value field`` () =
    let r = reader [| 0xC2uy; 0x3Auy; 0x12uy |]

    match run r trace (ValueRaw.parse 0x0Duy) with
    | Ok (ValueRaw.PosBcd payload) ->
        let raw =
            parsedRawField (ValueRaw.PosBcd payload) r.Position

        match Value.fromRaw raw with
        | Failed (failures, _) ->
            match Failures.toList failures with
            | [ issue ] ->
                issue.FieldId |> should equal payload.Id
                issue.Message |> should equal "Invalid BCD value: 3A12"

            | issues ->
                failwith $"Expected one validation issue, got %A{issues}"

        | actual ->
            failwith $"Expected validation failure, got %A{actual}"

    | Ok value ->
        failwith $"Expected positive BCD payload, got %A{value}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``unsupported data field returns structured parser error`` () =
    let r = reader [||]

    match run r trace (ValueRaw.parse 0x0Fuy) with
    | Error error ->
        error.Msg.Contains("invalid data field") |> should be True

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"
