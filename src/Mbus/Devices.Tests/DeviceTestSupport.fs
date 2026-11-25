module Metering.Mbus.Devices.Tests.DeviceTestSupport

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Codec
open Metering.Mbus.Devices
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages

module CoreDecoder = Metering.Common.Decoding.Decoders.Core.Decoder

let address = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]

let userData records = RspUdData.create records None false

let private resolver = {
    new IExternalSecurityContextResolver with
        member _.ResolveMode5Context _ = None
}

let run decoder (bytes: byte[]) =
    let store =
        InMemorySourceStore(fun data offset -> ByteReaderFactory.Create(data, offset))
        :> ISourceStore

    let root = store.AddRoot "device test" (ReadOnlyMemory bytes) false
    let mutable nextId = 0

    let trace = {
        new IFieldTracer with
            member _.SourceCreated _ = ()

            member _.BeginField(_, _) =
                nextId <- nextId + 1
                FieldId.create nextId

            member _.EndField(_, _) = ()
            member _.FailField(_, _) = ()
    }

    let source = {
        Id = FieldId.create 0
        Span = {
            Source = root.Id
            Offset = 0
            Length = bytes.Length
        }
        Value = ReadOnlyMemory bytes
    }

    let context = {
        CreateReader = fun data offset -> ByteReaderFactory.Create(data, offset)
        Sources = store
        Trace = trace
    }

    match decoder source context with
    | Decoded(value, _) -> value
    | actual -> failwith $"Unexpected decoder result: {actual}"

let decode = Decoder.create resolver

let frame bytes =
    match decode (ReadOnlyMemory bytes) with
    | Decoded(message, _) -> message
    | actual -> failwith $"Expected supported message, got {actual}"

let identity =
    let raw =
        run (CoreDecoder.parse SelectionCodec.parse) (Array.append [| 0x52uy |] address)

    let fields =
        match raw.Value with
        | DeviceSelectionRaw.BySecondaryAddress fields -> fields
        | _ -> failwith "Expected secondary address"

    match
        DeviceIdentification.validateElements fields.IdNum fields.Mfr fields.Version fields.DevType
    with
    | Passed(device, _) -> DeviceIdentification.toMeterAddress device
    | actual -> failwith $"Unexpected identity validation: {actual}"

let primaryAddress = PrimaryAddress.PrimAdr(PrimAdr.tryCreate 5uy |> Option.get)

let initial () =
    DeviceLogic.initialState primaryAddress identity

let shortFrame control address = [|
    0x10uy
    control
    address
    byte ((int control + int address) &&& 255)
    0x16uy
|]

let selectionFrame control address pattern =
    let userData = Array.concat [ [| control; address; 0x52uy |]; pattern ]
    let length = byte userData.Length
    let checksum = userData |> Array.sumBy int |> fun total -> byte (total &&& 255)
    Array.concat [ [| 0x68uy; length; length; 0x68uy |]; userData; [| checksum; 0x16uy |] ]

let select pattern =
    selectionFrame 0x53uy 0xFDuy pattern |> frame
