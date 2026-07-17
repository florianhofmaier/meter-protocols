module Metering.Common.Decoding.Parsers.Tests.ParserRunnerTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Tests.TestSupport

[<Fact>]
let ``run permits trailing bytes`` () =
    let r = reader [| 0xAAuy; 0xBBuy |]

    match run r trace parseU8 with
    | Ok value ->
        value |> should equal 0xAAuy
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``runExactly rejects trailing bytes`` () =
    let r = reader [| 0xAAuy; 0xBBuy; 0xCCuy |]

    match runExactly r trace parseU8 with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 1
        error.Msg.Contains("Exact parsing left trailing input") |> should be True
        error.Msg.Contains("2 byte(s) remaining") |> should be True

    | Ok value ->
        failwith $"Expected trailing input error, got {value:X2}"

[<Fact>]
let ``runExactly accepts full consumption`` () =
    let r = reader [| 0xAAuy |]

    match runExactly r trace parseU8 with
    | Ok value ->
        value |> should equal 0xAAuy

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``runExactlyWithSource preserves source id in trailing byte error`` () =
    let source = SourceId.create 42
    let r = reader [| 0xAAuy; 0xBBuy |]

    match runExactlyWithSource source r trace parseU8 with
    | Error error ->
        error.Source |> should equal source

    | Ok value ->
        failwith $"Expected trailing input error, got {value:X2}"

[<Fact>]
let ``runExactlyWithSource reports absolute reader position for non-zero base offset`` () =
    let source = SourceId.create 42
    let r = readerAt 10 [| 0xAAuy; 0xBBuy |]

    match runExactlyWithSource source r trace parseU8 with
    | Error error ->
        error.Pos |> should equal 11

    | Ok value ->
        failwith $"Expected trailing input error, got {value:X2}"

[<Fact>]
let ``decoder parseValue uses exact parser semantics`` () =
    let sourceId = SourceId.create 7

    let source =
        {
            Id = FieldId.create 12
            Span =
                {
                    Source = sourceId
                    Offset = 25
                    Length = 2
                }
            Value = ReadOnlyMemory<byte>([| 0xAAuy; 0xBBuy |])
        }

    let context =
        {
            CreateReader = fun bytes offset -> ByteReaderFactory.Create(bytes, offset)
            Sources = None
            Trace = trace
        }

    match Core.parseValue parseU8 source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Source |> should equal sourceId
        error.Pos |> should equal 26
        error.Msg.Contains("Exact parsing left trailing input") |> should be True

    | actual ->
        failwith $"Expected decoder parse failure, got %A{actual}"
