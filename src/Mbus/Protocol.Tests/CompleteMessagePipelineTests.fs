module Metering.Mbus.Protocol.Tests.CompleteMessagePipelineTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Security
open Metering.Mbus.Protocol.Tests.TestSupport

let private frame cField address higherLayerData =
    let userData =
        Array.concat [ [| cField; address |]; higherLayerData ]

    let length = byte userData.Length
    let checksum = userData |> Array.fold (+) 0uy

    Array.concat [
        [| 0x68uy; length; length; 0x68uy |]
        userData
        [| checksum; 0x16uy |]
    ]

let private decode securityContext bytes =
    let store =
        InMemorySourceStore(fun memory offset ->
            Metering.Common.Decoding.ByteReaders.ByteReaderFactory.Create(memory, offset))

    (store :> ISourceStore).AddRoot "M-Bus test frame" (memory bytes) false
    |> ignore

    WiredMbusFrame.decode
        securityContext
        (bytesField bytes)
        (decoderContext (store :> ISourceStore))

let private mode5Context bytes =
    match Mode5Key.create (ReadOnlyMemory<byte> bytes) with
    | Ok key -> SecurityContext.mode5 key
    | Error error -> failwith $"Invalid test key: %A{error}"

let private mode5LongHeader configuration =
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

[<Fact>]
let ``mode zero complete message owns a decoded APL`` () =
    let bytes =
        frame 0x53uy 0x01uy [| 0x51uy |]

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage message ->
            match message.Payload with
            | AplContent.Decoded _ -> ()
            | actual -> failwith $"Expected decoded APL, got %A{actual}"

    | actual ->
        failwith $"Expected decoded complete message, got %A{actual}"

[<Fact>]
let ``mode five without usable context retains protected source and partial suffix`` () =
    let longHeader = mode5LongHeader 0x0510us

    let encrypted = Array.zeroCreate<byte> 16
    let suffix = [| 0x0Fuy |]
    let bytes =
        frame 0x08uy 0x01uy (Array.concat [ longHeader; encrypted; suffix ])

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage message ->
            match message.Payload with
            | AplContent.Protected protectedApl ->
                protectedApl.Failure |> should equal SecurityContextNotUsable
                protectedApl.EncryptedPart.Value.Length |> should equal 16

                match protectedApl.ClearSuffix with
                | Some clearSuffix ->
                    clearSuffix.Value.ToArray() |> should equal suffix
                    clearSuffix.Span.Offset
                    |> should equal (
                        protectedApl.EncryptedPart.Span.Offset
                        + protectedApl.EncryptedPart.Span.Length
                    )

                | None ->
                    failwith "Expected retained clear suffix"

            | actual ->
                failwith $"Expected protected APL, got %A{actual}"

    | actual ->
        failwith $"Expected protected partial-encryption message, got %A{actual}"

[<Fact>]
let ``mode five with usable context decrypts and validates the APL`` () =
    let key =
        [| 0x00uy .. 0x0Fuy |]

    let cipherText =
        [|
            0x23uy; 0xE4uy; 0x18uy; 0x2Cuy
            0x34uy; 0x40uy; 0xD7uy; 0x2Auy
            0x04uy; 0x8Auy; 0x7Cuy; 0x6Cuy
            0x36uy; 0xE9uy; 0xCAuy; 0xBEuy
        |]

    let bytes =
        frame
            0x08uy
            0x01uy
            (Array.append (mode5LongHeader 0x05F0us) cipherText)

    match decode (mode5Context key) bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage message ->
            match message.Payload with
            | AplContent.Decoded apl ->
                apl.Span.Source.Value |> should not' (equal 0)

            | actual ->
                failwith $"Expected decoded APL, got %A{actual}"

    | actual ->
        failwith $"Expected decoded Mode 5 message, got %A{actual}"

[<Fact>]
let ``AFL CI is classified as unsupported standard conformant`` () =
    let bytes =
        frame 0x08uy 0x01uy [| 0x90uy |]

    match decode SecurityContext.none bytes with
    | DecodeFailed (ParseFailed error, _) ->
        error.Msg.Contains("Unsupported but standard-conformant AFL")
        |> should be True

        error.Msg.Contains("Table 2")
        |> should be True

    | actual ->
        failwith $"Expected unsupported AFL failure, got %A{actual}"
