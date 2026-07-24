module Metering.Common.Decoding.Parsers.Tests.ParserRunnerTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types

[<Fact>]
let ``runWithSource supplies requested source to parser errors`` () =
    let source =
        SourceId.create 42

    match runWithSource source (reader [||]) trace (Metering.Common.Decoding.Parsers.ErrorHandling.fail<int> "bad") with
    | Error error ->
        error.Source |> should equal source
        error.Pos |> should equal 0
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``unknown source from byte reader is defaulted to runner source`` () =
    let source =
        SourceId.create 42

    match runWithSource source (reader [| 0xAAuy |]) trace (Metering.Common.Decoding.Parsers.Utility.take 2) with
    | Error error ->
        error.Source |> should equal source
        error.Pos |> should equal 0
        error.Msg.Contains("Unexpected end of buffer") |> should equal true

    | Ok value ->
        failwith $"Expected parser error, got %A{value.ToArray()}"

[<Fact>]
let ``explicit parser error source is not overwritten`` () =
    let explicitSource =
        SourceId.create 99

    let parser : Parser<int> =
        fun _ ->
            raise (ParserException { Source = explicitSource; Pos = 3; Msg = "bad" })

    match runWithSource (SourceId.create 42) (reader [||]) trace parser with
    | Error error ->
        error.Source |> should equal explicitSource
        error.Pos |> should equal 3

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``prefix runner permits trailing bytes`` () =
    let r = reader [| 0xAAuy; 0xBBuy |]

    match run r trace parseU8 with
    | Ok value ->
        value |> should equal 0xAAuy
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``exact runner accepts full consumption`` () =
    let r = reader [| 0xAAuy |]

    match runExactly r trace parseU8 with
    | Ok value ->
        value |> should equal 0xAAuy
        r.Remaining |> should equal 0

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``exact runner rejects trailing bytes and reports source position and remaining count`` () =
    let source =
        SourceId.create 42

    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy; 0xCCuy |]

    match runExactlyWithSource source r trace parseU8 with
    | Error error ->
        error.Source |> should equal source
        error.Pos |> should equal 11
        error.Msg |> should equal "Exact parsing left trailing input. 2 byte(s) remaining."

    | Ok value ->
        failwith $"Expected trailing input error, got {value:X2}"

[<Fact>]
let ``exact runner works with empty parser and empty input`` () =
    match runExactly (reader [||]) trace (parser { return 42 }) with
    | Ok value ->
        value |> should equal 42

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``exact runner rejects zero-consumption parser on non-empty input`` () =
    match runExactly (reader [| 0xAAuy |]) trace (parser { return 42 }) with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 0
        error.Msg |> should equal "Exact parsing left trailing input. 1 byte(s) remaining."

    | Ok value ->
        failwith $"Expected trailing input error, got {value}"

[<Fact>]
let ``parser exceptions become error parser error`` () =
    let parser : Parser<int> =
        fun _ ->
            raise (ParserException { Source = SourceId.unknown; Pos = 5; Msg = "bad" })

    match runExactly (reader [||]) trace parser with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 5
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``non-parser programming exceptions escape`` () =
    let parser : Parser<int> =
        fun _ ->
            raise (InvalidOperationException "programming")

    let ex =
        Assert.Throws<InvalidOperationException>(fun () ->
            runExactly (reader [||]) trace parser |> ignore)

    ex.Message |> should equal "programming"

[<Fact>]
let ``supplied tracer is visible to the parser`` () =
    let tracer =
        RecordingTracer(7)

    let parser : Parser<IFieldTracer> =
        fun ctx -> ctx.Trace

    match runExactly (reader [||]) (tracer :> IFieldTracer) parser with
    | Ok visibleTracer ->
        Object.ReferenceEquals(visibleTracer, tracer :> IFieldTracer) |> should equal true

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"
