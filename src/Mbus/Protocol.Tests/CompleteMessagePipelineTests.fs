module Metering.Mbus.Protocol.Tests.CompleteMessagePipelineTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Messages
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

let private longHeaderMode0 id mfr version deviceType configuration apl =
    Array.concat [
        [| 0x72uy |]
        id
        mfr
        [|
            version
            deviceType
            0x08uy
            0x00uy
            byte configuration
            byte (configuration >>> 8)
        |]
        apl
    ]

let private failureMessages =
    function
    | DecodeFailed (ValidationFailed failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)
    | actual ->
        failwith $"Expected validation failure, got %A{actual}"

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
let ``partial mode five uses one derived APL source`` () =
    let key = [| 0x00uy .. 0x0Fuy |]
    let encryptedFillers =
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
            (Array.concat [
                mode5LongHeader 0x0510us
                encryptedFillers
                [| 0x2Fuy |]
            ])

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
            | actual -> failwith $"Expected decoded partial APL, got %A{actual}"
    | actual -> failwith $"Expected decoded partial message, got %A{actual}"

[<Fact>]
let ``wrong mode five key returns protected cryptographic failure`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (Array.append
                (mode5LongHeader 0x05F0us)
                (Array.zeroCreate 16))

    match decode (mode5Context (Array.create 16 0xAAuy)) bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage message ->
            match message.Payload with
            | AplContent.Protected protectedApl ->
                match protectedApl.Failure with
                | UnprotectionFailure.CryptographicFailure
                    DecryptionOrVerificationFailed -> ()
                | actual ->
                    failwith $"Expected cryptographic failure, got %A{actual}"
            | actual -> failwith $"Expected cryptographic protected payload, got %A{actual}"
    | actual -> failwith $"Expected protected message, got %A{actual}"

[<Fact>]
let ``root validation accumulates DLL and multiple TPL header issues`` () =
    let higherLayer =
        longHeaderMode0
            [| 0xFAuy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0xFFuy; 0x01uy |]
            0xFFuy
            0xFFuy
            0x0004us
            [| 0x2Fuy |]

    let bytes = frame 0x09uy 0xFCuy higherLayer
    bytes[bytes.Length - 2] <- bytes[bytes.Length - 2] + 1uy
    bytes[bytes.Length - 1] <- 0x00uy

    let messages =
        decode SecurityContext.none bytes
        |> failureMessages

    [
        "Invalid function code"
        "Invalid AField"
        "CRC mismatch"
        "End field is expected"
        "Invalid BCD nibble"
        "Wildcard byte 0xFF is not allowed in manufacturer"
        "Wildcard byte 0xFF is not allowed in version"
        "Wildcard byte 0xFF is not allowed in device type"
        "Invalid ContentOfMessage"
    ]
    |> List.iter (fun expected ->
        messages
        |> List.exists (fun message -> message.Contains(expected))
        |> should be True)

[<Fact>]
let ``unsupported security mode survives parsing and accumulates validation issues`` () =
    let higherLayer =
        longHeaderMode0
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0100us
            [| 0x2Fuy |]

    let bytes = frame 0x09uy 0x01uy higherLayer
    let messages =
        decode SecurityContext.none bytes
        |> failureMessages

    messages
    |> List.filter (fun message -> message.Contains("Security mode 1"))
    |> List.length
    |> should be (greaterThanOrEqualTo 2)

    messages
    |> List.exists (fun message -> message.Contains("Invalid function code"))
    |> should be True

[<Fact>]
let ``short header mode five is standard conformant but lacks wired IV material`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (Array.concat [
                [|
                    0x7Auy
                    0x08uy
                    0x00uy
                    0xF0uy; 0x05uy
                |]
                Array.zeroCreate 16
            ])

    let messages =
        decode SecurityContext.none bytes
        |> failureMessages

    messages
    |> List.exists (fun message ->
        message.Contains("standard-conformant")
        && message.Contains("Table 48"))
    |> should be True

[<Fact>]
let ``pure DLL parser preserves CI span and does not parse TPL`` () =
    let bytes =
        frame 0x08uy 0x01uy [| 0xFFuy; 0xAAuy |]

    let raw =
        parseExactly DllVariableLengthRaw.parse bytes

    let higher = raw.Value.UserData.Value.HigherLayerData
    higher.Value.ToArray() |> should equal [| 0xFFuy; 0xAAuy |]
    higher.Span.Offset |> should equal 6
    higher.Span.Length |> should equal 2

[<Fact>]
let ``decoded RSP UD is restored through all message adapters`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (longHeaderMode0
                [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
                [| 0x00uy; 0x01uy |]
                0x06uy
                0x07uy
                0x0000us
                [| 0x2Fuy |])

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (decodedFrame, _) ->
        ResponseUserData.matchesFrame decodedFrame |> should be True

        ResponseUserData.fromFrame decodedFrame
        |> validationValue
        |> fun response -> response.AccessNumber
        |> AccessNumber.value
        |> should equal 0x08uy

        SecondaryStationMessage.fromFrame decodedFrame
        |> validationValue
        |> function
            | SecondaryStationMessage.ResponseUserData _ -> ()
            | actual -> failwith $"Expected secondary response, got %A{actual}"

        Message.fromFrame decodedFrame
        |> validationValue
        |> function
            | Message.SecondaryStationMessage
                (SecondaryStationMessage.ResponseUserData _) -> ()
            | actual -> failwith $"Expected message response, got %A{actual}"
    | actual -> failwith $"Expected decoded frame, got %A{actual}"

[<Fact>]
let ``protected RSP UD is not mapped to decoded response data`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (Array.append
                (mode5LongHeader 0x05F0us)
                (Array.zeroCreate 16))

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (decodedFrame, _) ->
        ResponseUserData.matchesFrame decodedFrame |> should be False

        match ResponseUserData.fromFrame decodedFrame with
        | Failed (failures, _) ->
            failures
            |> Failures.toList
            |> List.exists (fun issue ->
                issue.Message.Contains("protected and unavailable"))
            |> should be True
        | actual -> failwith $"Expected protected-response mapping failure, got %A{actual}"
    | actual -> failwith $"Expected valid protected frame, got %A{actual}"

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
