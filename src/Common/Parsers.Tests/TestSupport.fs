module Metering.Common.Decoding.Parsers.Tests.TestSupport

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Parsers.Types

type NoopTracer() =
    interface IFieldTracer with
        member _.SourceCreated _ = ()

        member _.BeginField(_, _) =
            FieldId.create 0

        member _.EndField(_, _) = ()

        member _.FailField(_, _) = ()

let trace =
    NoopTracer() :> IFieldTracer

let reader (bytes: byte[]) =
    ByteReaderFactory.Create(ReadOnlyMemory<byte>(bytes))

let readerAt offset (bytes: byte[]) =
    ByteReaderFactory.Create(ReadOnlyMemory<byte>(bytes), offset)

