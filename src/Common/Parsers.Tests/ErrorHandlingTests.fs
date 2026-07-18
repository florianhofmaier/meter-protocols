module Metering.Common.Decoding.Parsers.Tests.ErrorHandlingTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types

[<Fact>]
let ``fail uses current source absolute position and message`` () =
    let source =
        SourceId.create 42

    let r =
        readerAt 10 [| 0xAAuy |]

    let parser =
        parser {
            let! _ = Metering.Common.Decoding.Parsers.Utility.take 1
            return! fail "bad"
        }

    match runWithSource source r trace parser with
    | Error error ->
        error.Source |> should equal source
        error.Pos |> should equal 11
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``failBefore reports current absolute position minus supplied count`` () =
    let source =
        SourceId.create 42

    let r =
        readerAt 10 [| 0xAAuy; 0xBBuy |]

    let parser =
        parser {
            let! _ = Metering.Common.Decoding.Parsers.Utility.take 2
            return! failBefore 2 "bad"
        }

    match runWithSource source r trace parser with
    | Error error ->
        error.Source |> should equal source
        error.Pos |> should equal 10
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got %A{value}"

[<Fact>]
let ``parser errors are returned as parser errors`` () =
    match runExactly (reader [||]) trace (fail<int> "bad") with
    | Error error ->
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got {value}"

[<Fact>]
let ``parser failures do not escape as unrelated exceptions`` () =
    let result =
        runExactly (reader [||]) trace (fail<int> "bad")

    match result with
    | Error error ->
        error.Msg |> should equal "bad"

    | Ok value ->
        failwith $"Expected parser error, got {value}"
