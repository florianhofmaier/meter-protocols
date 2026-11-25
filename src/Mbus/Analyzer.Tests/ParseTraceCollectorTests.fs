module Metering.Mbus.Analysis.Tests.ParseTraceCollectorTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Mbus.Analysis
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Parsers.Types

[<Fact>]
let ``snapshots preserve open fields and do not change when parsing completes`` () =
    let collector = ParseTraceCollector()
    let trace = collector :> IFieldTracer

    let start = {
        Source = SourceId.create 3
        Offset = 7
        Length = 0
    }

    let id = trace.BeginField("payload", start)
    let before = collector.Snapshot()

    let span = { start with Length = 2 }
    trace.EndField(id, span)
    let after = collector.Snapshot()

    before.Fields.Head.Result |> should equal (ParseFieldResult.Reading start)
    after.Fields.Head.Result |> should equal (ParseFieldResult.Parsed span)
    after.Fields.Head.Id |> should equal id

[<Fact>]
let ``sources keep their origin and subsequent parsing forms a separate root`` () =
    let collector = ParseTraceCollector()
    let trace = collector :> IFieldTracer

    let store =
        InMemorySourceStore(fun bytes offset -> ByteReaderFactory.Create(bytes, offset))
        :> ISourceStore

    let root = store.AddRoot "frame" (ReadOnlyMemory<byte> [| 0x10uy; 0x20uy |]) false
    trace.SourceCreated root

    let origin = {
        Source = root.Id
        Offset = 1
        Length = 1
    }

    let first = trace.BeginField("cipher", { origin with Length = 0 })
    trace.EndField(first, origin)

    let derived =
        store.AddDerived
            "plain"
            origin
            (SourceTransform.Decrypt "test")
            (ReadOnlyMemory<byte> [| 0x30uy |])
            true

    trace.SourceCreated derived

    let plain = {
        Source = derived.Id
        Offset = 0
        Length = 1
    }

    let second = trace.BeginField("plain payload", { plain with Length = 0 })
    trace.EndField(second, plain)

    let report = collector.Snapshot()
    report.Sources |> should equal [ root; derived ]
    derived.Origin |> should equal (Some origin)
    report.Fields |> List.map _.Id |> should equal [ first; second ]

    report.Fields
    |> List.forall (fun field -> field.Children.IsEmpty)
    |> should be True

    first |> should not' (equal second)
