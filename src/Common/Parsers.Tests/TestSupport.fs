module Metering.Common.Decoding.Parsers.Tests.TestSupport

open System
open Metering.Common.Decoding.ByteReaders
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
