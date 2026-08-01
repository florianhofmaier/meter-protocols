module Metering.Mbus.Protocol.Tests.CompleteMessagePipelineTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
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

let private shortHeader ci configuration apl =
    Array.concat [
        [|
            ci
            0x08uy
            0x00uy
            byte configuration
            byte (configuration >>> 8)
        |]
        apl
    ]

let private longHeaderMode0WithCi ci id mfr version deviceType configuration apl =
    Array.concat [
        [| ci |]
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

let private longHeaderMode0 id mfr version deviceType configuration apl =
    longHeaderMode0WithCi
        0x72uy
        id
        mfr
        version
        deviceType
        configuration
        apl

let private failureMessages =
    function
    | DecodeFailed (ValidationFailed failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)
    | actual ->
        failwith $"Expected validation failure, got %A{actual}"

let private parseFailure =
    function
    | DecodeFailed (ParseFailed error, _) -> error
    | actual -> failwith $"Expected parse failure, got %A{actual}"

[<Fact>]
let ``valid primary command direction passes and owns a decoded APL`` () =
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
                | UnprotectionIssue.CryptographicFailure
                    CryptographicFailure.DecryptionOrVerificationFailed -> ()
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
let ``standard defined unsupported security mode stops parsing at configuration`` () =
    let higherLayer =
        longHeaderMode0
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0100us
            [| 0x2Fuy |]

    frame 0x09uy 0x01uy higherLayer
    |> decode SecurityContext.none
    |> parseFailure
    |> fun error ->
        error.Msg.Contains("Unsupported, but standard-conformant security mode 1")
        |> should be True

[<Fact>]
let ``reserved security mode stops parsing at configuration`` () =
    let higherLayer =
        longHeaderMode0
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0600us
            [| 0x2Fuy |]

    frame 0x08uy 0x01uy higherLayer
    |> decode SecurityContext.none
    |> parseFailure
    |> fun error ->
        error.Msg.Contains("Reserved/standard-invalid security mode value 6")
        |> should be True

[<Fact>]
let ``wired short header mode zero is structurally accepted`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (shortHeader 0x7Auy 0x0000us [||])

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage {
            Tpl = { Value = Tpl.ShortHeader _ }
            Payload = AplContent.Decoded _
          } -> ()

        | actual ->
            failwith $"Expected decoded short-header complete message, got %A{actual}"

    | actual ->
        failwith $"Expected structurally accepted Mode 0 short header, got %A{actual}"

[<Fact>]
let ``wired short header mode five fails in root validation`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (shortHeader
                0x7Auy
                0x05F0us
                (Array.zeroCreate 16))

    let messages = decode SecurityContext.none bytes |> failureMessages

    [ "requires a long TPL header"; "short TPL header"; "security mode: 5"; "Table 48" ]
    |> List.iter (fun expected ->
        messages
        |> List.exists _.Contains(expected)
        |> should be True)

    messages
    |> List.exists _.Contains("SecurityContextNotUsable")
    |> should be False

[<Fact>]
let ``wired CI 0x57 short header mode five fails in root validation`` () =
    let bytes =
        frame
            0x53uy
            0x01uy
            (shortHeader
                0x57uy
                0x05F0us
                (Array.zeroCreate 16))

    let messages = decode SecurityContext.none bytes |> failureMessages

    [ "requires a long TPL header"; "short TPL header"; "security mode: 5"; "9.4.4"; "Table 48" ]
    |> List.iter (fun expected ->
        messages
        |> List.exists _.Contains(expected)
        |> should be True)

    [ "SecurityContextNotUsable"; "authentication"; "decryption"; "AFL" ]
    |> List.iter (fun forbidden ->
        messages
        |> List.exists _.Contains(forbidden)
        |> should be False)

[<Fact>]
let ``CI 0x57 selects ApplicationResetOrSelect APL parser and passes primary direction`` () =
    let bytes =
        frame
            0x53uy
            0x01uy
            (shortHeader 0x57uy 0x0000us [||])

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded (
        WiredMbusFrame.VariableLength field,
        _
      ) ->
        match field.Value with
        | FrameVariableLength.CompleteMessage message ->
            match message.Payload with
            | AplContent.Decoded apl ->
                match apl.Value with
                | Metering.Mbus.Protocol.Frames.ApplicationLayer.Apl.ApplicationResetOrSelect
                    Metering.Mbus.Protocol.Frames.ApplicationLayer.ApplicationReset -> ()

                | actual ->
                    failwith $"Expected application reset APL, got %A{actual}"

            | actual ->
                failwith $"Expected decoded APL for CI 0x57, got %A{actual}"

    | actual -> failwith $"Expected valid CI 0x57 command, got %A{actual}"

[<Fact>]
let ``short header unsupported mode stops parsing at configuration`` () =
    let higherLayer =
        shortHeader 0x7Auy 0x0100us [| 0x2Fuy |]

    frame 0x08uy 0x01uy higherLayer
    |> decode SecurityContext.none
    |> parseFailure
    |> fun error ->
        error.Msg.Contains("Unsupported, but standard-conformant security mode 1")
        |> should be True

[<Fact>]
let ``CI 0x5A from EN 13757-7 Table 2 is standard-defined unsupported`` () =
    let bytes =
        frame 0x53uy 0x01uy [| 0x5Auy |]

    match decode SecurityContext.none bytes with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 6

        [
            "Unsupported, but standard-conformant wired TPL CI value 0x5A"
            "applicable to wired M-Bus"
            "Field: CI-Field TPL"
            "Supported TPL CI values"
            "EN 13757-7:2018, 5.2, Table 2"
        ]
        |> List.iter (fun expected ->
            error.Msg.Contains(expected)
            |> should be True)

    | actual ->
        failwith $"Expected standard-defined unsupported CI failure, got %A{actual}"

[<Fact>]
let ``CI 0x91 from EN 13757-7 Table 2 is reserved`` () =
    let bytes =
        frame 0x08uy 0x01uy [| 0x91uy |]

    match decode SecurityContext.none bytes with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 6

        [
            "Reserved TPL CI value 0x91"
            "Field: CI-Field TPL"
            "Supported TPL CI values"
            "EN 13757-7:2018, 5.2, Table 2"
        ]
        |> List.iter (fun expected ->
            error.Msg.Contains(expected)
            |> should be True)

    | actual ->
        failwith $"Expected reserved CI parser failure, got %A{actual}"

let private assertCurrentWiredApplicationCiUnsupported ci =
    let bytes =
        frame 0x53uy 0x01uy [| ci |]

    match decode SecurityContext.none bytes with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 6

        [
            $"Unsupported, but standard-conformant wired TPL CI value 0x{ci:X2}"
            "applicable to wired M-Bus"
            "Field: CI-Field TPL"
            "Supported TPL CI values"
            "EN 13757-3:2025"
            "Clause 7.3"
            "Table 27"
        ]
        |> List.iter (fun expected ->
            error.Msg.Contains(expected)
            |> should be True)

        [
            "Unknown"
            "Reserved"
            "AFL"
        ]
        |> List.iter (fun forbidden ->
            error.Msg.Contains(forbidden)
            |> should be False)

    | actual ->
        failwith $"Expected wired-applicable unsupported CI 0x{ci:X2} failure, got %A{actual}"

[<Fact>]
let ``CI 0x54 is standard-defined and wired-applicable but unsupported`` () =
    assertCurrentWiredApplicationCiUnsupported 0x54uy

[<Fact>]
let ``CI 0x55 is standard-defined and wired-applicable but unsupported`` () =
    assertCurrentWiredApplicationCiUnsupported 0x55uy

[<Fact>]
let ``CI 0x56 is standard-defined and wired-applicable but unsupported`` () =
    assertCurrentWiredApplicationCiUnsupported 0x56uy

[<Fact>]
let ``CI 0x67 is standard-defined but not applicable to wired M-Bus`` () =
    let bytes =
        frame 0x08uy 0x01uy [| 0x67uy |]

    match decode SecurityContext.none bytes with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 6

        [
            "TPL CI value 0x67 is standard-defined but not applicable to wired M-Bus"
            "Field: CI-Field TPL"
            "Supported TPL CI values"
            "EN 13757-3:2025"
            "Clause 7.3"
            "Table 27"
        ]
        |> List.iter (fun expected ->
            error.Msg.Contains(expected)
            |> should be True)

        [
            "Unsupported, but standard-conformant wired TPL CI"
            "Reserved"
            "Unknown"
            "AFL"
        ]
        |> List.iter (fun forbidden ->
            error.Msg.Contains(forbidden)
            |> should be False)

    | actual ->
        failwith $"Expected not-applicable-to-wired CI failure, got %A{actual}"

[<Fact>]
let ``CI 0x57 with secondary response C-field fails direction validation`` () =
    let messages =
        frame
            0x08uy
            0x01uy
            (shortHeader 0x57uy 0x0000us [||])
        |> decode SecurityContext.none
        |> failureMessages

    messages
    |> List.exists (fun message ->
        message.Contains("Cross-layer direction mismatch")
        && message.Contains("C-field=0x08")
        && message.Contains("CI=0x57"))
    |> should be True

[<Fact>]
let ``CI 0x7A with primary command C-field fails direction validation`` () =
    let messages =
        frame
            0x53uy
            0x01uy
            (shortHeader 0x7Auy 0x0000us [||])
        |> decode SecurityContext.none
        |> failureMessages

    messages
    |> List.exists (fun message ->
        message.Contains("Cross-layer direction mismatch")
        && message.Contains("C-field=0x53")
        && message.Contains("CI=0x7A"))
    |> should be True

[<Fact>]
let ``CI 0x7A with secondary response C-field passes direction validation`` () =
    let bytes =
        frame
            0x08uy
            0x01uy
            (shortHeader 0x7Auy 0x0000us [||])

    match decode SecurityContext.none bytes with
    | Metering.Common.Decoding.Decoders.Core.Decoded _ -> ()
    | actual -> failwith $"Expected valid CI 0x7A secondary response, got %A{actual}"

[<Fact>]
let ``secondary response C-field with command CI fails cross-layer validation`` () =
    let messages =
        frame 0x08uy 0x01uy [| 0x51uy |]
        |> decode SecurityContext.none
        |> failureMessages

    messages
    |> List.exists (fun message ->
        message.Contains("Cross-layer direction mismatch")
        && message.Contains("secondary response")
        && message.Contains("CI=0x51"))
    |> should be True

[<Fact>]
let ``primary command C-field with response CI fails cross-layer validation`` () =
    let higherLayer =
        longHeaderMode0
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0000us
            [| 0x2Fuy |]

    let messages =
        frame 0x53uy 0x01uy higherLayer
        |> decode SecurityContext.none
        |> failureMessages

    messages
    |> List.exists (fun message ->
        message.Contains("Cross-layer direction mismatch")
        && message.Contains("primary command")
        && message.Contains("CI=0x72"))
    |> should be True

[<Fact>]
let ``CI 0x53 with primary command C-field passes direction validation`` () =
    let higherLayer =
        longHeaderMode0WithCi
            0x53uy
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0000us
            [||]

    match frame 0x53uy 0x01uy higherLayer |> decode SecurityContext.none with
    | Metering.Common.Decoding.Decoders.Core.Decoded _ -> ()
    | actual -> failwith $"Expected valid CI 0x53 primary command, got %A{actual}"

[<Fact>]
let ``CI 0x72 with secondary response C-field passes direction validation`` () =
    let higherLayer =
        longHeaderMode0
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0000us
            [| 0x2Fuy |]

    match frame 0x08uy 0x01uy higherLayer |> decode SecurityContext.none with
    | Metering.Common.Decoding.Decoders.Core.Decoded _ -> ()
    | actual -> failwith $"Expected valid secondary response, got %A{actual}"

[<Fact>]
let ``root validation accumulates local cross-layer and APL issues`` () =
    let higherLayer =
        longHeaderMode0WithCi
            0x53uy
            [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]
            [| 0x00uy; 0x01uy |]
            0x06uy
            0x07uy
            0x0004us
            [| 0x00uy .. 0x0Auy |]

    let messages =
        frame 0x09uy 0x01uy higherLayer
        |> decode SecurityContext.none
        |> failureMessages

    [
        "Invalid function code"
        "Invalid ContentOfMessage"
        "Cross-layer direction mismatch"
        "at most 10 byte(s)"
    ]
    |> List.iter (fun expected ->
        messages
        |> List.exists (fun message -> message.Contains(expected))
        |> should be True)

[<Fact>]
let ``pure DLL parser preserves CI span and does not parse TPL`` () =
    let bytes =
        frame 0x08uy 0x01uy [| 0xFFuy; 0xAAuy |]

    let raw =
        parseExactly VariableLengthFrameRaw.parse bytes

    let higher = raw.Value.UserData.Value.LinkUserData
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

        error.Msg.Contains("TPL CI value")
        |> should be False

        error.Msg.Contains("Reserved TPL CI")
        |> should be False

        error.Msg.Contains("Unknown TPL CI")
        |> should be False

    | actual ->
        failwith $"Expected unsupported AFL failure, got %A{actual}"
