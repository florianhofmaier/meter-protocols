module Metering.Common.Decoding.Parsers.Tests.FieldTests

open System
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

let private field =
    {
        Id = FieldId.create 3
        Span =
            {
                Source = SourceId.create 4
                Offset = 5
                Length = 6
            }
        Value = 7
    }

[<Fact>]
let ``Field helpers preserve identity and span while transforming value`` () =
    Field.value field |> should equal 7
    Field.id field |> should equal (FieldId.create 3)
    Field.span field |> should equal field.Span

    let mapped =
        Field.map ((+) 1) field

    mapped.Id |> should equal field.Id
    mapped.Span |> should equal field.Span
    mapped.Value |> should equal 8

    let replaced =
        Field.withValue "value" field

    replaced.Id |> should equal field.Id
    replaced.Span |> should equal field.Span
    replaced.Value |> should equal "value"

[<Fact>]
let ``Field helpers preserve the same contract`` () =
    let parsed : Field<int> =
        field

    Field.value parsed |> should equal 7
    Field.id parsed |> should equal field.Id
    Field.span parsed |> should equal field.Span

    let mapped =
        Field.map string parsed

    mapped.Id |> should equal field.Id
    mapped.Span |> should equal field.Span
    mapped.Value |> should equal "7"

    let replaced =
        Field.withValue 9 parsed

    replaced.Id |> should equal field.Id
    replaced.Span |> should equal field.Span
    replaced.Value |> should equal 9

[<Fact>]
let ``parseField traces begin and end with returned field span`` () =
    let tracer =
        RecordingTracer(20)

    let r =
        readerAt 10 [| 0xAAuy |]

    match runExactlyWithSource (SourceId.create 9) r (tracer :> IFieldTracer) (parseField "byte" parseU8) with
    | Ok field ->
        field.Id |> should equal (FieldId.create 20)
        field.Span |> should equal { Source = SourceId.create 9; Offset = 10; Length = 1 }
        field.Value |> should equal 0xAAuy

        tracer.Events
        |> should equal [
            BeginField ("byte", { Source = SourceId.create 9; Offset = 10; Length = 0 }, FieldId.create 20)
            EndField (FieldId.create 20, field.Span)
        ]

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``parseField supports zero-length inner parsers`` () =
    let tracer =
        RecordingTracer()

    match runExactly (reader [||]) (tracer :> IFieldTracer) (parseField "empty" (parser { return 1 })) with
    | Ok field ->
        field.Span.Length |> should equal 0

        tracer.Events
        |> should equal [
            BeginField ("empty", { Source = SourceId.root; Offset = 0; Length = 0 }, FieldId.create 0)
            EndField (FieldId.create 0, { Source = SourceId.root; Offset = 0; Length = 0 })
        ]

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``parseField preserves non-zero reader base offsets`` () =
    let tracer =
        RecordingTracer()

    match runExactly (readerAt 50 [| 0xAAuy |]) (tracer :> IFieldTracer) (parseField "byte" parseU8) with
    | Ok field ->
        field.Span.Offset |> should equal 50
        field.Span.Length |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``nested fields produce ordered events and spans`` () =
    let tracer =
        RecordingTracer()

    let parser =
        parseField
            "outer"
            (parser {
                let! inner = parseField "inner" parseU8
                let! second = parseU8
                return inner.Value, second
            })

    match runExactly (readerAt 10 [| 0xAAuy; 0xBBuy |]) (tracer :> IFieldTracer) parser with
    | Ok outer ->
        outer.Span |> should equal { Source = SourceId.root; Offset = 10; Length = 2 }

        tracer.Events
        |> should equal [
            BeginField ("outer", { Source = SourceId.root; Offset = 10; Length = 0 }, FieldId.create 0)
            BeginField ("inner", { Source = SourceId.root; Offset = 10; Length = 0 }, FieldId.create 1)
            EndField (FieldId.create 1, { Source = SourceId.root; Offset = 10; Length = 1 })
            EndField (FieldId.create 0, { Source = SourceId.root; Offset = 10; Length = 2 })
        ]

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``parseField traces parser failures without end event`` () =
    let tracer =
        RecordingTracer()

    match runExactly (reader [||]) (tracer :> IFieldTracer) (parseField "bad" (Metering.Common.Decoding.Parsers.ErrorHandling.fail<int> "bad")) with
    | Error error ->
        error.Msg |> should equal "bad"

        tracer.Events
        |> should equal [
            BeginField ("bad", { Source = SourceId.root; Offset = 0; Length = 0 }, FieldId.create 0)
            FailField (FieldId.create 0, { Source = SourceId.root; Pos = 0; Msg = "bad" })
        ]

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``parseField replaces unknown error source with parser context source`` () =
    let tracer =
        RecordingTracer()

    let parser : Parser<int> =
        failWith { Source = SourceId.unknown; Pos = 12; Msg = "bad" }

    match runExactlyWithSource (SourceId.create 7) (reader [||]) (tracer :> IFieldTracer) (parseField "bad" parser) with
    | Error error ->
        error.Source |> should equal (SourceId.create 7)

        match tracer.Events with
        | [ BeginField _; FailField (_, failError) ] ->
            failError.Source |> should equal (SourceId.create 7)

        | events ->
            failwith $"Unexpected events %A{events}"

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``parseField preserves explicit parser error source`` () =
    let tracer =
        RecordingTracer()

    let parser : Parser<int> =
        failWith { Source = SourceId.create 99; Pos = 12; Msg = "bad" }

    match runExactlyWithSource (SourceId.create 7) (reader [||]) (tracer :> IFieldTracer) (parseField "bad" parser) with
    | Error error ->
        error.Source |> should equal (SourceId.create 99)

        match tracer.Events with
        | [ BeginField _; FailField (_, failError) ] ->
            failError.Source |> should equal (SourceId.create 99)

        | events ->
            failwith $"Unexpected events %A{events}"

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``parseField does not swallow non-parser programming exceptions`` () =
    let parser : Parser<int> =
        fun _ ->
            raise (InvalidOperationException "programming")

    let ex =
        Assert.Throws<InvalidOperationException>(fun () ->
            runExactly (reader [||]) trace (parseField "bad" parser) |> ignore)

    ex.Message |> should equal "programming"
