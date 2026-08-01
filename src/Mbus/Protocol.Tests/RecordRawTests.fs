module Metering.Mbus.Protocol.Tests.RecordRawTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Tests.TestSupport

let private peekU8 reader =
    match reader.Peek 1 with
    | Ok bytes -> bytes.Span[0]
    | Error error -> failwith error.Msg

[<Fact>]
let ``DIF 08 parses as selection and leaves following bytes`` () =
    let r = reader [| 0x08uy; 0x00uy; 0x99uy |]

    match run r trace RecordRaw.parse with
    | Ok (RecordRaw.Selection selection) ->
        selection.Value.Dib.Span.Length |> should equal 1
        selection.Value.Vib.Span.Length |> should equal 1
        r.Position |> should equal 2
        r.Remaining |> should equal 1
        peekU8 r |> should equal 0x99uy

    | Ok record ->
        failwith $"Expected selection record, got %A{record}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``DIF 88 with DIFE parses as selection with extended DIB`` () =
    let r = reader [| 0x88uy; 0x01uy; 0x00uy; 0x99uy |]

    match run r trace RecordRaw.parse with
    | Ok (RecordRaw.Selection selection) ->
        selection.Value.Dib.Span.Length |> should equal 2
        selection.Value.Vib.Span.Length |> should equal 1
        r.Position |> should equal 3
        r.Remaining |> should equal 1
        peekU8 r |> should equal 0x99uy

    | Ok record ->
        failwith $"Expected selection record, got %A{record}"

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``truncated selection record returns structured parser error`` () =
    let r = reader [| 0x08uy |]

    match run r trace RecordRaw.parse with
    | Error error ->
        error.Msg.Contains("Unexpected end of buffer") |> should be True

    | Ok record ->
        failwith $"Expected parser error, got %A{record}"
