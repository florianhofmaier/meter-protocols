module Metering.Mbus.DecodePipeline.Tests.TestSupport

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.Decoding
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type NoopTracer() =
    interface IFieldTracer with
        member _.SourceCreated _ = ()
        member _.BeginField(_, _) = FieldId.create 0
        member _.EndField(_, _) = ()
        member _.FailField(_, _) = ()

type NoSecurityContext() =
    interface IExternalSecurityContextResolver with
        member _.ResolveMode5Context(_identity: DeviceIdentification) = None

type FixedMode5Context(key: byte[]) =
    let secret = Secret128.TakeOwnership(Array.copy key)

    interface IExternalSecurityContextResolver with
        member _.ResolveMode5Context(_identity: DeviceIdentification) =
            Some (Mode5ExternalContext secret)

let private memory (bytes: byte[]) =
    ReadOnlyMemory<byte> bytes

let frame cField address higherLayerData =
    let userData =
        Array.concat [ [| cField; address |]; higherLayerData ]

    let length = byte userData.Length
    let checksum = userData |> Array.fold (+) 0uy

    Array.concat [
        [| 0x68uy; length; length; 0x68uy |]
        userData
        [| checksum; 0x16uy |]
    ]

let mode5LongHeader (configuration: uint16) =
    [|
        0x72uy
        0x02uy; 0x03uy; 0x04uy; 0x05uy
        0x00uy; 0x01uy
        0x06uy
        0x07uy
        0x08uy
        0x00uy
        byte configuration; byte (configuration >>> 8)
    |]

let decode resolver bytes =
    let store =
        InMemorySourceStore(fun value offset ->
            ByteReaderFactory.Create(value, offset))

    let root =
        (store :> ISourceStore).AddRoot "M-Bus test frame" (memory bytes) false

    let source = {
        Id = FieldId.create 1
        Span = {
            Source = root.Id
            Offset = 0
            Length = bytes.Length
        }
        Value = memory bytes
    }

    let context = {
        CreateReader = fun value offset -> ByteReaderFactory.Create(value, offset)
        Sources = store
        Trace = NoopTracer()
    }

    match WiredVariableFrameDecode.decode resolver source context with
    | Decoded (value, _) -> value
    | DecodeFailed (failure, _) -> failwith $"Unexpected outer decoder failure: {failure}"
