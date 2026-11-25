module Metering.Mbus.Analysis.Tests.ParseTraceTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Mbus.Analysis
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Mbus.Protocol.Frames.TransportLayer

let private parseTpl (bytes: byte[]) =
    let store =
        InMemorySourceStore(fun bytes offset -> ByteReaderFactory.Create(bytes, offset))
        :> ISourceStore

    let collector = ParseTraceCollector()
    let trace = collector :> IFieldTracer
    let root = store.AddRoot "TPL input" (ReadOnlyMemory<byte> bytes) false
    trace.SourceCreated root

    let source = {
        Id = FieldId.create -1
        Span = {
            Source = root.Id
            Offset = 0
            Length = bytes.Length
        }
        Value = ReadOnlyMemory<byte> bytes
    }

    let context = {
        CreateReader = fun bytes offset -> ByteReaderFactory.Create(bytes, offset)
        Sources = store
        Trace = trace
    }

    let result = Decoder.parse TplCodec.parse source context
    root, store, collector.Snapshot(), result

[<Fact>]
let ``truncated long TPL header keeps child spans for interpretation from the source`` () =
    // EN 13757-7:2018, 7.4, Table 12, p. 26: two-byte configuration field.
    let bytes = [|
        0x72uy
        0x78uy
        0x56uy
        0x34uy
        0x12uy
        0x2Duy
        0x2Cuy
        0x01uy
        0x07uy
        0x08uy
        0x00uy
        0x10uy
    |]

    let root, store, report, result = parseTpl bytes

    let error =
        match result with
        | DecodeFailed(ParseFailed error, _) -> error
        | actual -> failwith $"Expected truncated configuration field, got {actual}"

    error.Source |> should equal root.Id
    error.Pos |> should equal 11
    report.Sources |> should equal [ root ]

    let tpl = Assert.Single report.Fields
    let header = tpl.Children |> List.find (fun field -> field.Name = "Long TPL Header")

    let access =
        header.Children |> List.find (fun field -> field.Name = "Access Number")

    let configuration =
        header.Children |> List.find (fun field -> field.Name = "Configuration Field")

    let start offset = {
        Source = root.Id
        Offset = offset
        Length = 0
    }

    tpl.Result |> should equal (ParseFieldResult.Failed(start 0, error))
    header.Result |> should equal (ParseFieldResult.Failed(start 1, error))
    configuration.Result |> should equal (ParseFieldResult.Failed(start 11, error))

    header.Children
    |> List.filter (fun field ->
        match field.Result with
        | ParseFieldResult.Parsed _ -> true
        | _ -> false)
    |> List.length
    |> should equal 6

    let span =
        match access.Result with
        | ParseFieldResult.Parsed span -> span
        | actual -> failwith $"Expected completed access-number field, got {actual}"

    span
    |> should equal {
        Source = root.Id
        Offset = 9
        Length = 1
    }

    // Interpret the original, bounded source bytes with the existing field parser.
    let reader = store.CreateReader span.Source

    match reader.Skip span.Offset with
    | Ok() -> ()
    | Error error -> failwith $"Expected available child offset, got {error}"

    let bounded =
        match reader.Slice span.Length with
        | Ok reader -> reader
        | Error error -> failwith $"Expected available child bytes, got {error}"

    let interpretationTrace = ParseTraceCollector() :> IFieldTracer

    match
        ParserRunner.runExactlyWithSource
            span.Source
            bounded
            store
            interpretationTrace
            AccessNumberRaw.parse
    with
    | Ok access ->
        AccessNumberRaw.value access.Value |> should equal 8uy
        access.Span |> should equal span
    | Error error -> failwith $"Expected access-number interpretation, got {error}"

[<Fact>]
let ``successful TPL trace retains the parser field identities and full spans`` () =
    let bytes = [|
        0x72uy
        0x78uy
        0x56uy
        0x34uy
        0x12uy
        0x2Duy
        0x2Cuy
        0x01uy
        0x07uy
        0x08uy
        0x00uy
        0x00uy
        0x00uy
        0xFFuy
    |]

    let _, _, report, result = parseTpl bytes

    match result with
    | Decoded(raw, _) ->
        let tpl = Assert.Single report.Fields
        tpl.Id |> should equal raw.Id
        tpl.Result |> should equal (ParseFieldResult.Parsed raw.Span)

        match raw.Value with
        | TplRaw.LongHeader raw ->
            let header = tpl.Children |> List.find (fun field -> field.Name = "Long TPL Header")
            header.Id |> should equal raw.Header.Id
            header.Result |> should equal (ParseFieldResult.Parsed raw.Header.Span)
            let payload = tpl.Children |> List.find (fun field -> field.Name = "APL Data")
            payload.Id |> should equal raw.AplData.Id
            payload.Result |> should equal (ParseFieldResult.Parsed raw.AplData.Span)
        | actual -> failwith $"Expected long TPL header, got {actual}"
    | actual -> failwith $"Expected successful TPL parsing, got {actual}"
