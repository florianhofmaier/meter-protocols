module Metering.Dlms.DecodePipeline.Tests.TestSupport

open System
open System.Security.Cryptography
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Dlms.Protocol.CosemApdus.Decoding

type NoopTracer() =
    interface IFieldTracer with
        member _.SourceCreated _ = ()
        member _.BeginField(_, _) = FieldId.create 0
        member _.EndField(_, _) = ()
        member _.FailField(_, _) = ()

type NoKeys() =
    interface IGlobalCipherKeyResolver with
        member _.ResolveGlobalKeys _ = None

type FixedKeys(encryptionKey: byte[], authenticationKey: byte[]) =
    interface IGlobalCipherKeyResolver with
        member _.ResolveGlobalKeys _ =
            Some {
                GlobalUnicastEncryptionKey = ReadOnlyMemory<byte> encryptionKey
                GlobalBroadcastEncryptionKey = None
                AuthenticationKey = ReadOnlyMemory<byte> authenticationKey
            }

let private encodeLength length =
    if length >= 0x80 then
        invalidArg (nameof length) "Test helper supports only short-form lengths"

    byte length

let private tagged tag (content: byte[]) =
    Array.concat [ [| tag; encodeLength content.Length |]; content ]

let private applicationContext contextId =
    [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x01uy; contextId |]
    |> tagged 0x06uy
    |> tagged 0xA1uy

let private callingApTitle (systemTitle: byte[]) =
    systemTitle
    |> tagged 0x04uy
    |> tagged 0xA6uy

let private userInformation (bytes: byte[]) =
    bytes
    |> tagged 0x04uy
    |> tagged 0xBEuy

let aarq includeApplicationContext systemTitle userInformationBytes =
    let fields =
        [
            if includeApplicationContext then
                applicationContext 0x03uy

            callingApTitle systemTitle
            userInformation userInformationBytes
        ]

    fields
    |> Array.concat
    |> tagged 0x60uy

let wrapper (version: uint16) (data: byte[]) =
    Array.concat [
        [|
            byte (version >>> 8)
            byte version
            0x00uy; 0x01uy
            0x00uy; 0x10uy
            byte (data.Length >>> 8)
            byte data.Length
        |]
        data
    ]

let initiateRequest =
    [|
        0x01uy
        0x00uy
        0x00uy
        0x00uy
        0x06uy
        0x5Fuy; 0x1Fuy; 0x04uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy
        0xFFuy; 0xFFuy
    |]

let private invocationCounterBytes (invocationCounter: uint32) =
    [|
        byte (invocationCounter >>> 24)
        byte (invocationCounter >>> 16)
        byte (invocationCounter >>> 8)
        byte invocationCounter
    |]

let protectedInitiateRequest
    (systemTitle: byte[])
    (encryptionKey: byte[])
    (authenticationKey: byte[])
    (invocationCounter: uint32)
    =

    let securityControl = 0x30uy
    let invocationCounter = invocationCounterBytes invocationCounter
    let iv = Array.concat [ systemTitle; invocationCounter ]
    let aad = Array.append [| securityControl |] authenticationKey
    let cipherText = Array.zeroCreate<byte> initiateRequest.Length
    let authenticationTag = Array.zeroCreate<byte> 12

    use aes = new AesGcm(encryptionKey, authenticationTag.Length)

    aes.Encrypt(
        iv,
        initiateRequest,
        cipherText,
        authenticationTag,
        aad
    )

    Array.concat [
        [|
            0x21uy
            encodeLength (1 + invocationCounter.Length + cipherText.Length + authenticationTag.Length)
            securityControl
        |]
        invocationCounter
        cipherText
        authenticationTag
    ]

let invalidProtectedInitiateRequest invocationCounter =
    let invocationCounter = invocationCounterBytes invocationCounter

    Array.concat [
        [| 0x21uy; 0x11uy; 0x30uy |]
        invocationCounter
        Array.zeroCreate<byte> 12
    ]

let decode resolver bytes =
    let store =
        InMemorySourceStore(fun value offset ->
            ByteReaderFactory.Create(value, offset))

    let memory = ReadOnlyMemory<byte> bytes

    let root =
        (store :> ISourceStore).AddRoot "DLMS TCP wrapper" memory false

    let source = {
        Id = FieldId.create 1
        Span = {
            Source = root.Id
            Offset = 0
            Length = bytes.Length
        }
        Value = memory
    }

    let context = {
        CreateReader = fun value offset -> ByteReaderFactory.Create(value, offset)
        Sources = store
        Trace = NoopTracer()
    }

    match TcpAarqDecode.decode resolver source context with
    | Decoded (value, _) -> value
    | DecodeFailed (failure, _) ->
        failwith $"Unexpected outer decoder failure: {failure}"
