module Metering.Mbus.Protocol.Tests.Mode5Tests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security
open Metering.Mbus.Protocol.Tests.TestSupport

type CapturingSourceStore() =
    let mutable derivedBytes : ReadOnlyMemory<byte> option = None

    member _.DerivedBytes =
        derivedBytes

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
            derivedBytes <- Some bytes

            {
                Id = SourceId.create 101
                Name = name
                Origin = Some origin
                Transform = transform
                Length = bytes.Length
                Sensitive = sensitive
            }

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

let private cnf blocks =
    let blockCount =
        NumberOfEncryptedBlocks.tryCreate blocks
        |> Option.get

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
                NumberOfEncryptedBlocks = blockCount
                Mode = Mode.Mode5
                Synchronized = false
                Accessibility = false
                BidirectionalCommunication = false
            }
    }

let private runUnprotect key blocks payload =
    let store = CapturingSourceStore()

    let result =
        Mode5.unprotect
            (SecurityContext.mode5 (memory key))
            testDevice
            testAccessNumber
            (cnf blocks)
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

[<Fact>]
let ``one encrypted block means sixteen encrypted bytes`` () =
    let result, _ =
        runUnprotect [||] 1 (Array.zeroCreate 16)

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``two encrypted blocks mean thirty-two encrypted bytes`` () =
    let result, _ =
        runUnprotect [||] 2 (Array.zeroCreate 32)

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 0 byte(s)"

[<Fact>]
let ``declared encrypted length greater than available payload is validation failure`` () =
    let result, _ =
        runUnprotect validKey 2 (Array.zeroCreate 31)

    let messages =
        expectValidationFailure result

    messages
    |> List.exists (fun message ->
        message.Contains("Declared encrypted length is 32 byte(s)")
        && message.Contains("31 byte(s) are available"))
    |> should be True

[<Fact>]
let ``unencrypted suffix reports standard-defined unsupported partial encryption`` () =
    let result, _ =
        runUnprotect validKey 1 (Array.zeroCreate 17)

    let messages =
        expectValidationFailure result

    messages
    |> List.exists (fun message ->
        message.Contains("standard-defined but currently unsupported")
        && message.Contains("16 byte(s)")
        && message.Contains("17 byte(s)"))
    |> should be True

[<Fact>]
let ``invalid verification bytes fail as encryption failure`` () =
    let result, _ =
        runUnprotect validKey 1 invalidCheckCipherText

    expectEncryptionFailure result
    |> should equal "security mode 5 AES check failed"

[<Fact>]
let ``valid fixed AES-CBC vector strips verification bytes from returned source`` () =
    let result, store =
        runUnprotect validKey 1 validCipherText

    match result with
    | Decoded (apl, _) ->
        apl.Value.ToArray() |> should equal expectedApplicationPlaintext
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
        runUnprotect [| 0x00uy |] 1 validCipherText

    expectEncryptionFailure result
    |> should equal "invalid AES-CBC key length: 1 byte(s)"
