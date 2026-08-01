module Metering.Mbus.Protocol.Tests.TestSupport

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core

type NoopTracer() =
    interface IFieldTracer with
        member _.SourceCreated _ = ()

        member _.BeginField(_, _) =
            FieldId.create 0

        member _.EndField(_, _) = ()

        member _.FailField(_, _) = ()

let trace =
    NoopTracer() :> IFieldTracer

let memory (bytes: byte[]) =
    ReadOnlyMemory<byte>(bytes)

let reader (bytes: byte[]) =
    ByteReaderFactory.Create(memory bytes)

let run (reader: IByteReader) trace parser =
    let store =
        InMemorySourceStore(fun value offset ->
            ByteReaderFactory.Create(value, offset))

    (store :> ISourceStore).AddRoot "parser test" reader.Buffer false
    |> ignore

    ParserRunner.run reader (store :> ISourceStore) trace parser

let parseResult parser bytes =
    let store =
        InMemorySourceStore(fun value offset ->
            ByteReaderFactory.Create(value, offset))

    let root =
        (store :> ISourceStore).AddRoot "parser test" (memory bytes) false

    ParserRunner.runExactlyWithSource
        root.Id
        (reader bytes)
        (store :> ISourceStore)
        trace
        parser

let parseExactly parser bytes =
    match parseResult parser bytes with
    | Ok value -> value
    | Error error -> failwith $"Unexpected parser error: {error.Msg}"

let bytesField (bytes: byte[]) =
    {
        Id = FieldId.create 1
        Span =
            {
                Source = SourceId.root
                Offset = 0
                Length = bytes.Length
            }
        Value = memory bytes
    }

let validationValue =
    function
    | Passed (value, _) ->
        value

    | Failed (failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)
        |> String.concat "; "
        |> failwith

let validationMessages =
    function
    | ValidationFailed failures ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)

    | failure ->
        failwith $"Expected validation failure, got %A{failure}"

let decoderContext (sourceStore: ISourceStore) =
    {
        CreateReader = fun bytes offset -> ByteReaderFactory.Create(bytes, offset)
        Sources = sourceStore
        Trace = trace
    }
