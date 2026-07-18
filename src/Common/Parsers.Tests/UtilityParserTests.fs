module Metering.Common.Decoding.Parsers.Tests.UtilityParserTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility

let private runOk parser bytes =
    match runExactly (reader bytes) trace parser with
    | Ok value ->
        value

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``expect succeeds when values match`` () =
    runOk (expect parseU8 0xAAuy) [| 0xAAuy |]
    |> should equal ()

[<Fact>]
let ``expect mismatch consumes compared value and reports current position`` () =
    let r =
        reader [| 0xBBuy; 0xCCuy |]

    match run r trace (expect parseU8 0xAAuy) with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 1
        error.Msg |> should equal "expected 170, but got 187"
        r.Position |> should equal 1
        r.Remaining |> should equal 1

    | Ok () ->
        failwith "Expected parser error"

[<Fact>]
let ``skip remaining position take and takeAll expose reader state`` () =
    let parser =
        parser {
            let! start = position
            let! first = take 1
            do! skip 1
            let! left = remaining
            let! rest = takeAll
            let! endPos = position
            return start, first.ToArray(), left, rest.ToArray(), endPos
        }

    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy; 0xCCuy |]

    match runExactly r trace parser with
    | Ok value ->
        value |> should equal (10, [| 0xAAuy |], 1, [| 0xCCuy |], 13)

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``bufferSliceAt uses absolute offsets and does not move reader`` () =
    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy; 0xCCuy; 0xDDuy |]

    let parser =
        parser {
            do! skip 2
            let! earlier = bufferSliceAt 10 2
            let! middle = bufferSliceAt 11 2
            let! pos = position
            return earlier.ToArray(), middle.ToArray(), pos
        }

    match run r trace parser with
    | Ok value ->
        value |> should equal ([| 0xAAuy; 0xBBuy |], [| 0xBBuy; 0xCCuy |], 12)
        r.Position |> should equal 12

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Theory>]
[<InlineData(10, -1, "length -1")>]
[<InlineData(9, 1, "absolute offset 9")>]
[<InlineData(13, 2, "absolute offset 13")>]
let ``bufferSliceAt invalid bounds return structured parser errors`` start count (expectedMessage: string) =
    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy; 0xCCuy; 0xDDuy |]

    match run r trace (bufferSliceAt start count) with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 10
        error.Msg.Contains(expectedMessage) |> should equal true
        r.Position |> should equal 10

    | Ok bytes ->
        failwith $"Expected parser error, got %A{bytes.ToArray()}"

[<Fact>]
let ``runOnSubSlice consumes parent only when child fully consumes slice`` () =
    let r =
        reader [| 0xAAuy; 0xBBuy; 0xCCuy |]

    match run r trace (runOnSubSlice 2 (take 2)) with
    | Ok bytes ->
        bytes.ToArray() |> should equal [| 0xAAuy; 0xBBuy |]
        r.Position |> should equal 2
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``runOnSubSlice under-consumption returns structured error without advancing parent`` () =
    let r =
        reader [| 0xAAuy; 0xBBuy |]

    match run r trace (runOnSubSlice 2 (take 1)) with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 1
        error.Msg |> should equal "Sub-slice not fully consumed. 1 byte(s) remaining."
        r.Position |> should equal 0

    | Ok bytes ->
        failwith $"Expected parser error, got %A{bytes.ToArray()}"

[<Fact>]
let ``runOnSubSlice over-read returns truncation error without advancing parent`` () =
    let r =
        reader [| 0xAAuy; 0xBBuy |]

    match run r trace (runOnSubSlice 2 (take 3)) with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 0
        error.Msg.Contains("Unexpected end of buffer") |> should equal true
        r.Position |> should equal 0

    | Ok bytes ->
        failwith $"Expected parser error, got %A{bytes.ToArray()}"

[<Fact>]
let ``nested fields inside a sub-slice use absolute spans`` () =
    let tracer =
        RecordingTracer()

    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy |]

    match run r (tracer :> IFieldTracer) (runOnSubSlice 2 (parseField "inner" (take 2))) with
    | Ok field ->
        field.Span |> should equal { Source = SourceId.root; Offset = 10; Length = 2 }

        tracer.Events
        |> should equal [
            BeginField ("inner", { Source = SourceId.root; Offset = 10; Length = 0 }, FieldId.create 0)
            EndField (FieldId.create 0, { Source = SourceId.root; Offset = 10; Length = 2 })
        ]

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``parseUntilEnd returns empty list for empty input`` () =
    runOk (parseUntilEnd parseU8) [||]
    |> should equal ([]: byte list)

[<Fact>]
let ``parseUntilEnd preserves order and parses until exact exhaustion`` () =
    runOk (parseUntilEnd parseU8) [| 0x01uy; 0x02uy; 0x03uy |]
    |> should equal [ 0x01uy; 0x02uy; 0x03uy ]

[<Fact>]
let ``parseUntilEnd rejects parser that consumes no bytes`` () =
    match run (reader [| 0xAAuy |]) trace (parseUntilEnd (parser { return 1 })) with
    | Error error ->
        error.Pos |> should equal 0
        error.Msg |> should equal "Parser did not consume any input in parseUntilEnd."

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``parseUntilEnd propagates inner parser errors`` () =
    match run (reader [| 0xAAuy |]) trace (parseUntilEnd (fail<int> "inner")) with
    | Error error ->
        error.Msg |> should equal "inner"

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"
