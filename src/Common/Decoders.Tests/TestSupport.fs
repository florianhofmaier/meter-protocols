module Metering.Common.Decoding.Decoders.Tests.TestSupport

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core

type TraceEvent =
    | SourceCreated of SourceInfo
    | BeginField of string * SourceSpan * FieldId
    | EndField of FieldId * SourceSpan
    | FailField of FieldId * ParserError

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

type ReaderFactoryCall =
    {
        Bytes : byte[]
        Offset : int
    }

type RecordingReaderFactory() =
    let calls =
        ResizeArray<ReaderFactoryCall>()

    member _.Calls =
        calls |> Seq.toList

    member _.Create(bytes: ReadOnlyMemory<byte>) offset =
        calls.Add({ Bytes = bytes.ToArray(); Offset = offset })
        ByteReaderFactory.Create(bytes, offset)

type CapturingSourceStore(sourceId: SourceId) =
    let addDerivedCalls =
        ResizeArray<string * SourceSpan * SourceTransform * byte[] * bool>()

    member _.AddDerivedArgs =
        addDerivedCalls
        |> Seq.tryLast

    member _.AddDerivedCalls =
        addDerivedCalls
        |> Seq.toList

    interface ISourceStore with
        member _.AddRoot name bytes sensitive =
            {
                Id = sourceId
                Name = name
                Origin = None
                Transform = SourceTransform.Root
                Length = bytes.Length
                Sensitive = sensitive
            }

        member _.AddDerived name origin transform bytes sensitive =
            addDerivedCalls.Add(name, origin, transform, bytes.ToArray(), sensitive)

            {
                Id = sourceId
                Name = name
                Origin = Some origin
                Transform = transform
                Length = bytes.Length
                Sensitive = sensitive
            }

        member _.CreateReader _ =
            ByteReaderFactory.Create(ReadOnlyMemory<byte>([||]), 0)

let memory (bytes: byte[]) =
    ReadOnlyMemory<byte>(bytes)

let sourceField fieldId sourceId offset (bytes: byte[]) : Field<ReadOnlyMemory<byte>> =
    {
        Id = FieldId.create fieldId
        Span =
            {
                Source = SourceId.create sourceId
                Offset = offset
                Length = Array.length bytes
            }
        Value = memory bytes
    }

let issue id message =
    {
        FieldId = FieldId.create id
        Message = message
    }

let context sourceStore (readerFactory: RecordingReaderFactory) (tracer: IFieldTracer) =
    {
        CreateReader = readerFactory.Create
        Sources = sourceStore
        Trace = tracer
    }

let notices =
    function
    | Decoded (_, notices)
    | DecodeFailed (_, notices) ->
        notices
