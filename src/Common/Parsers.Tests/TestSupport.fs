module Metering.Common.Decoding.Parsers.Tests.TestSupport

open System
open System.Collections.Generic
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types

type TraceEvent =
    | SourceCreated of SourceInfo
    | BeginField of string * SourceSpan * FieldId
    | EndField of FieldId * SourceSpan
    | FailField of FieldId * ParserError

type NoopTracer() =
    interface IFieldTracer with
        member _.SourceCreated _ = ()

        member _.BeginField(_, _) =
            FieldId.create 0

        member _.EndField(_, _) = ()

        member _.FailField(_, _) = ()

let trace =
    NoopTracer() :> IFieldTracer

type TestSourceStore() =
    let buffers =
        Dictionary<SourceId, ReadOnlyMemory<byte>>()

    let sources =
        ResizeArray<SourceInfo>()

    let mutable nextId =
        0

    let add
        name
        origin
        transform
        (bytes: ReadOnlyMemory<byte>)
        sensitive =

        let info: SourceInfo =
            {
                Id = SourceId.create nextId
                Name = name
                Origin = origin
                Transform = transform
                Length = bytes.Length
                Sensitive = sensitive
            }

        nextId <- nextId + 1
        buffers.Add(info.Id, bytes)
        sources.Add info
        info

    member _.Sources =
        sources |> Seq.toList

    interface ISourceStore with
        member _.AddRoot name bytes sensitive =
            add name None SourceTransform.Root bytes sensitive

        member _.AddDerived name origin transform bytes sensitive =
            add name (Some origin) transform bytes sensitive

        member _.CreateReader source =
            ByteReaderFactory.Create(buffers[source], 0)

let sources =
    TestSourceStore() :> ISourceStore

type RecordingTracer(?firstId: int) =
    let events =
        ResizeArray<TraceEvent>()

    let mutable nextId =
        defaultArg firstId 0

    member _.Events =
        events |> Seq.toList

    interface IFieldTracer with
        member _.SourceCreated source =
            events.Add(SourceCreated source)

        member _.BeginField(name, span) =
            let id =
                FieldId.create nextId

            nextId <- nextId + 1
            events.Add(BeginField (name, span, id))
            id

        member _.EndField(id, span) =
            events.Add(EndField (id, span))

        member _.FailField(id, error) =
            events.Add(FailField (id, error))

let memory (bytes: byte[]) =
    ReadOnlyMemory<byte>(bytes)

let reader (bytes: byte[]) =
    ByteReaderFactory.Create(memory bytes)

let readerAt offset (bytes: byte[]) =
    ByteReaderFactory.Create(memory bytes, offset)

let run reader trace parser =
    ParserRunner.run reader sources trace parser

let runWithSource source reader trace parser =
    ParserRunner.runWithSource source reader sources trace parser

let runExactly reader trace parser =
    ParserRunner.runExactly reader sources trace parser

let runExactlyWithSource source reader trace parser =
    ParserRunner.runExactlyWithSource source reader sources trace parser
