module Metering.Common.Decoding.Parsers.Tests.ParserSourceTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.ParserSource
open Metering.Common.Decoding.Parsers.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility

let private rootRuntime bytes =
    let store = TestSourceStore()
    let root =
        (store :> ISourceStore).AddRoot "root" (memory bytes) false
    store, root

let private field
    id
    source
    offset
    (bytes: byte[])
    : Field<ReadOnlyMemory<byte>> =

    {
        Id = FieldId.create id
        Span = { Source = source; Offset = offset; Length = bytes.Length }
        Value = memory bytes
    }

[<Fact>]
let ``createDerived registers one sensitive source and preserves origin identity`` () =
    let store, root = rootRuntime [| 0xAAuy; 0xBBuy |]
    let tracer = RecordingTracer()
    let origin = field 7 root.Id 0 [| 0xAAuy; 0xBBuy |]
    let derivedBytes = memory [| 0x01uy; 0x02uy; 0x03uy |]

    match
        ParserRunner.run
            ((store :> ISourceStore).CreateReader root.Id)
            (store :> ISourceStore)
            (tracer :> IFieldTracer)
            (createDerived
                "plaintext"
                (SourceTransform.Decrypt "AES")
                true
                origin
                derivedBytes)
    with
    | Ok derived ->
        derived.Id |> should equal origin.Id
        derived.Span.Source |> should not' (equal root.Id)
        derived.Span.Offset |> should equal 0
        derived.Span.Length |> should equal 3
        derived.Value.ToArray() |> should equal (derivedBytes.ToArray())

        store.Sources
        |> List.filter (fun source -> source.Origin.IsSome)
        |> should haveLength 1

        let info = store.Sources |> List.last
        info.Name |> should equal "plaintext"
        info.Origin |> should equal (Some origin.Span)
        info.Transform |> should equal (SourceTransform.Decrypt "AES")
        info.Length |> should equal 3
        info.Sensitive |> should be True

        tracer.Events |> should equal [ SourceCreated info ]
    | Error error -> failwith error.Msg

[<Fact>]
let ``parseExactly uses nested source identity and leaves outer reader unchanged`` () =
    let bytes = [| 0xAAuy; 0x10uy; 0x20uy; 0xBBuy |]
    let store, root = rootRuntime bytes
    let nested = field 3 root.Id 1 [| 0x10uy; 0x20uy |]
    let outerReader = (store :> ISourceStore).CreateReader root.Id

    let parser =
        parser {
            do! skip 1
            let! before = position
            let! parsed =
                parseExactly nested (parseField "nested" parseU16BigEndian)
            let! after = position
            return before, after, parsed
        }

    match
        ParserRunner.runWithSource
            root.Id
            outerReader
            (store :> ISourceStore)
            trace
            parser
    with
    | Ok (before, after, parsed) ->
        before |> should equal 1
        after |> should equal before
        parsed.Value |> should equal 0x1020us
        parsed.Span.Source |> should equal root.Id
        parsed.Span.Offset |> should equal 1
    | Error error -> failwith error.Msg

[<Fact>]
let ``parseExactly reports nested trailing input`` () =
    let store, root = rootRuntime [| 0xAAuy; 0xBBuy |]
    let nested = field 3 root.Id 0 [| 0xAAuy; 0xBBuy |]

    match
        ParserRunner.runWithSource
            root.Id
            ((store :> ISourceStore).CreateReader root.Id)
            (store :> ISourceStore)
            trace
            (parseExactly nested parseU8)
    with
    | Error error ->
        error.Source |> should equal root.Id
        error.Pos |> should equal 1
        error.Msg |> should equal "Exact parsing left trailing input. 1 byte(s) remaining."
    | Ok value -> failwith $"Expected trailing-input failure, got {value:X2}"

[<Fact>]
let ``parseExactly reports nested parser source and position`` () =
    let store, root = rootRuntime [| 0xAAuy |]
    let derived =
        (store :> ISourceStore).AddDerived
            "nested"
            { Source = root.Id; Offset = 0; Length = 1 }
            (SourceTransform.Decrypt "test")
            (memory [| 0xAAuy |])
            false

    let nested = field 3 derived.Id 0 [| 0xAAuy |]

    match
        ParserRunner.runWithSource
            root.Id
            ((store :> ISourceStore).CreateReader root.Id)
            (store :> ISourceStore)
            trace
            (parseExactly nested (take 2))
    with
    | Error error ->
        error.Source |> should equal derived.Id
        error.Pos |> should equal 0
        error.Msg.Contains("Unexpected end of buffer") |> should be True
    | Ok value -> failwith $"Expected nested failure, got %A{value.ToArray()}"
