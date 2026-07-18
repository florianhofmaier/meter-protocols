module Metering.Mbus.Protocol.Tests.Mode5Tests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security
open Metering.Mbus.Protocol.Tests.TestSupport

type CapturingSourceStore() =
    let mutable derivedBytes : ReadOnlyMemory<byte> option = None
    let mutable derivedSource : SourceInfo option = None

    member _.DerivedBytes =
        derivedBytes

    member _.DerivedSource =
        derivedSource

    interface ISourceStore with
        member _.AddRoot name bytes sensitive =
            {
                Id = SourceId.create 100
                Name = name
                Origin = None
                Transform = SourceTransform.Root
                Length = bytes.Length
                Sensitive = sensitive
            }

        member _.AddDerived name origin transform bytes sensitive =
            let source =
                {
                    Id = SourceId.create 101
                    Name = name
                    Origin = Some origin
                    Transform = transform
                    Length = bytes.Length
                    Sensitive = sensitive
                }

            derivedBytes <- Some bytes
            derivedSource <- Some source
            source

        member _.CreateReader _ =
            ByteReaderFactory.Create(ReadOnlyMemory<byte>([||]), 0)

let private validKey =
    [|
        0x00uy; 0x01uy; 0x02uy; 0x03uy
        0x04uy; 0x05uy; 0x06uy; 0x07uy
        0x08uy; 0x09uy; 0x0Auy; 0x0Buy
        0x0Cuy; 0x0Duy; 0x0Euy; 0x0Fuy
    |]

let private validCipherText =
    [|
        0x26uy; 0x06uy; 0xA6uy; 0xF4uy
        0xEDuy; 0x1Auy; 0x1Buy; 0x2Buy
        0x5Cuy; 0x89uy; 0x1Auy; 0xAEuy
        0x6Duy; 0x50uy; 0x8Fuy; 0x01uy
    |]

let private validTwoBlockCipherText =
    [|
        0xAAuy; 0xA8uy; 0x1Auy; 0xB2uy
        0x6Buy; 0xB2uy; 0x85uy; 0xAEuy
        0x3Duy; 0xA1uy; 0x0Auy; 0xD4uy
        0x81uy; 0xA1uy; 0xACuy; 0x2Cuy
        0x04uy; 0xF3uy; 0x2Cuy; 0xBEuy
        0x18uy; 0xAEuy; 0x84uy; 0x3Duy
        0x54uy; 0xE7uy; 0xFFuy; 0x4Cuy
        0x7Duy; 0x94uy; 0x8Auy; 0x74uy
    |]

let private invalidCheckCipherText =
    [|
        0x68uy; 0x7Duy; 0xD6uy; 0x3Euy
        0x6Fuy; 0xC9uy; 0x1Euy; 0xCFuy
        0xF1uy; 0xEDuy; 0xFCuy; 0x09uy
        0x52uy; 0x8Auy; 0x7Euy; 0xF1uy
    |]

let private expectedApplicationPlaintext =
    [|
        0x0Cuy; 0x13uy; 0x37uy; 0x00uy
        0x01uy; 0x02uy; 0x03uy; 0x04uy
        0x05uy; 0x06uy; 0x07uy; 0x08uy
        0x09uy; 0x0Auy
    |]

let private expectedTwoBlockApplicationPlaintext =
    [|
        0x00uy; 0x01uy; 0x02uy; 0x03uy
        0x04uy; 0x05uy; 0x06uy; 0x07uy
        0x08uy; 0x09uy; 0x0Auy; 0x0Buy
        0x0Cuy; 0x0Duy; 0x0Euy; 0x0Fuy
        0x10uy; 0x11uy; 0x12uy; 0x13uy
        0x14uy; 0x15uy; 0x16uy; 0x17uy
        0x18uy; 0x19uy; 0x1Auy; 0x1Buy
        0x1Cuy; 0x1Duy
    |]

let private testDevice =
    let idNum =
        parseExactly IdNumberRaw.parse [| 0x02uy; 0x03uy; 0x04uy; 0x05uy |]

    let mfr =
        parseExactly ManufacturerRaw.parse [| 0x00uy; 0x01uy |]

    let version =
        parseExactly VersionRaw.parse [| 0x06uy |]

    let devType =
        parseExactly DeviceTypeRaw.parse [| 0x07uy |]

    DeviceIdentification.fromRaw idNum mfr version devType
    |> validationValue

let private testAccessNumber =
    parseExactly AccessNumberRaw.parse [| 0x08uy |]
    |> AccessNumber.fromRaw
    |> validationValue

let private fixedBlocks count =
    match EncryptedBlockCount.tryCreate count with
    | Some blocks ->
        FixedEncryptedBlocks blocks

    | None ->
        failwith $"Invalid test fixed encrypted block count: {count}"

let private cnf encryptedLength =
    {
        Id = FieldId.create 2
        Span =
            {
                Source = SourceId.root
                Offset = 0
                Length = 2
            }
        Value =
            {
                HopCounter = false
                RepeaterAccess = false
                ContentOfMsg = ContentOfMessage.StandardData
                EncryptedLength = encryptedLength
                Mode = Mode.Mode5
                Synchronized = false
                Accessibility = false
                BidirectionalCommunication = false
            }
    }

let private runUnprotect key encryptedLength payload =
    let store = CapturingSourceStore()

    let result =
        Mode5.unprotect
            (SecurityContext.mode5 (memory key))
            testDevice
            testAccessNumber
            (cnf encryptedLength)
            (bytesField payload)
            (decoderContext (Some (store :> ISourceStore)))

    result, store

let private expectEncryptionFailure result =
    match result with
    | DecodeFailed (EncryptionFailed issue, _) ->
        issue.Message

    | actual ->
        failwith $"Expected encryption failure, got %A{actual}"

let private expectValidationFailure result =
    match result with
    | DecodeFailed (failure, _) ->
        validationMessages failure

    | actual ->
        failwith $"Expected validation failure, got %A{actual}"

let private expectSingleValidationMessage result =
    match expectValidationFailure result with
    | [ message ] ->
        message

    | messages ->
        failwith $"Expected one validation message, got %A{messages}"

[<Fact>]
let ``fixed count one resolves to sixteen encrypted bytes`` () =
    let result, _ =
        runUnprotect [||] (fixedBlocks 1) (Array.zeroCreate 16)

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``fixed count fourteen resolves to two hundred twenty-four encrypted bytes`` () =
    let result, _ =
        runUnprotect [||] (fixedBlocks 14) (Array.zeroCreate 224)

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``fixed count larger than available payload is validation failure`` () =
    let result, _ =
        runUnprotect validKey (fixedBlocks 2) (Array.zeroCreate 31)

    let messages =
        expectValidationFailure result

    messages
    |> List.exists (fun message ->
        message.Contains("Declared encrypted length is 32 byte(s)")
        && message.Contains("31 byte(s) are available"))
    |> should be True

[<Fact>]
let ``fixed count smaller than payload reports unsupported partial encryption`` () =
    let result, _ =
        runUnprotect validKey (fixedBlocks 1) (Array.zeroCreate 17)

    let message =
        expectSingleValidationMessage result

    message.Contains("standard-defined but currently unsupported")
    |> should be True

    message.Contains("16 byte(s)") |> should be True
    message.Contains("17 byte(s)") |> should be True

[<Fact>]
let ``fixed count equal to payload length uses fully encrypted path`` () =
    let result, _ =
        runUnprotect [||] (fixedBlocks 1) validCipherText

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``all remaining with sixteen byte vector decrypts successfully`` () =
    let result, _ =
        runUnprotect validKey AllRemainingDataEncrypted validCipherText

    match result with
    | Decoded (apl, _) ->
        apl.Value.ToArray() |> should equal expectedApplicationPlaintext

    | actual ->
        failwith $"Expected successful decryption, got %A{actual}"

[<Fact>]
let ``all remaining with thirty-two bytes uses the complete payload`` () =
    let result, store =
        runUnprotect validKey AllRemainingDataEncrypted validTwoBlockCipherText

    match result with
    | Decoded (apl, _) ->
        apl.Value.ToArray() |> should equal expectedTwoBlockApplicationPlaintext
        apl.Id |> should equal (FieldId.create 1)
        apl.Span.Source |> should equal (SourceId.create 101)
        apl.Span.Offset |> should equal 0
        apl.Span.Length |> should equal expectedTwoBlockApplicationPlaintext.Length

        match store.DerivedBytes with
        | Some bytes ->
            bytes.ToArray() |> should equal expectedTwoBlockApplicationPlaintext

        | None ->
            failwith "Expected derived source bytes"

        match store.DerivedSource with
        | Some source ->
            source.Id |> should equal (SourceId.create 101)
            source.Name |> should equal "Decrypted APL Data"
            source.Origin |> should equal (Some (bytesField validTwoBlockCipherText).Span)
            source.Transform |> should equal (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
            source.Length |> should equal expectedTwoBlockApplicationPlaintext.Length
            source.Sensitive |> should equal true

        | None ->
            failwith "Expected derived source metadata"

    | actual ->
        failwith $"Expected successful decryption, got %A{actual}"

[<Fact>]
let ``all remaining with more than two hundred forty bytes is not interpreted as fifteen blocks`` () =
    let result, _ =
        runUnprotect [||] AllRemainingDataEncrypted (Array.zeroCreate 256)

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``all remaining payload longer than two hundred forty bytes is not reported as partial encryption`` () =
    let result, _ =
        runUnprotect [||] AllRemainingDataEncrypted (Array.zeroCreate 256)

    match result with
    | DecodeFailed (ValidationFailed failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)
        |> List.exists (fun message -> message.Contains("partial encryption"))
        |> should equal false

    | DecodeFailed (EncryptionFailed _, _) ->
        ()

    | actual ->
        failwith $"Expected encryption or validation failure, got %A{actual}"

[<Fact>]
let ``all remaining with non block-aligned payload fails before cryptography`` () =
    let result, _ =
        runUnprotect [||] AllRemainingDataEncrypted (Array.zeroCreate 17)

    expectSingleValidationMessage result
    |> should equal "Security mode 5 all-remaining encrypted payload length must be a multiple of 16 byte(s), but got 17 byte(s)."

[<Fact>]
let ``invalid verification bytes fail as encryption failure`` () =
    let result, _ =
        runUnprotect validKey AllRemainingDataEncrypted invalidCheckCipherText

    expectEncryptionFailure result
    |> should equal "security mode 5 AES check failed"

[<Fact>]
let ``valid fixed AES-CBC vector strips verification bytes from returned source`` () =
    let result, store =
        runUnprotect validKey (fixedBlocks 1) validCipherText

    match result with
    | Decoded (apl, _) ->
        apl.Value.ToArray() |> should equal expectedApplicationPlaintext
        apl.Span.Source |> should equal (SourceId.create 101)
        apl.Span.Offset |> should equal 0
        apl.Span.Length |> should equal expectedApplicationPlaintext.Length

        match store.DerivedBytes with
        | Some bytes ->
            bytes.ToArray() |> should equal expectedApplicationPlaintext

        | None ->
            failwith "Expected derived source bytes"

    | actual ->
        failwith $"Expected successful decryption, got %A{actual}"

[<Fact>]
let ``invalid key length remains encryption failure`` () =
    let result, _ =
        runUnprotect [| 0x00uy |] (fixedBlocks 1) validCipherText

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 1 byte(s)"

[<Fact>]
let ``mode five with no encrypted data is explicitly unsupported`` () =
    let result, _ =
        runUnprotect validKey NoEncryptedData [||]

    expectSingleValidationMessage result
    |> should equal "Security mode 5 with no encrypted data is standard-defined but currently unsupported."
