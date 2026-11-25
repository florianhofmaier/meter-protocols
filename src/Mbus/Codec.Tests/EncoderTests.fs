module Metering.Mbus.Codec.Tests.EncoderTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Tests.TestSupport
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.WiredMbus

let private keyBytes = Convert.FromHexString "000102030405060708090A0B0C0D0E0F"

let private meter =
    let raw =
        parseExactly SelectionCodec.parse [|
            0x52uy
            0x78uy
            0x56uy
            0x34uy
            0x12uy
            0xE6uy
            0x1Euy
            0x3Cuy
            7uy
        |]

    match raw.Value with
    | DeviceSelectionRaw.BySecondaryAddress fields ->
        DeviceIdentification.validateElements fields.IdNum fields.Mfr fields.Version fields.DevType
        |> validationValue
        |> DeviceIdentification.toMeterAddress
    | _ -> failwith "meter identity"

let private mode5 = EncodingOptions.Mode5 { SecondaryAddress = meter }

let private otherMeter =
    let id =
        parseExactly IdNumberRaw.parse [| 0x21uy; 0x43uy; 0x65uy; 0x87uy |]
        |> IdNumber.validate
        |> validationValue
        |> _.Value

    { meter with IdNum = id }

let private data =
    let raw =
        parseExactly (FieldParser.parseField "Record" DataRecordRaw.parse) [| 2uy; 0uy; 42uy; 0uy |]

    DataRecordRsp.validate raw |> validationValue

let private response: ResponseUserData = {
    Address = AField.Configured(PrimAdr.tryCreate 5uy |> Option.get)
    Acd = false
    Dfc = false
    Status = {
        ApplicationError = ApplicationError.NoError
        PowerLow = false
        PermanentError = false
        TemporaryError = false
        Bit5 = false
        Bit6 = false
        Bit7 = false
    }
    MeterAddress = meter
    UserData = [ data ]
    MfrData = None
    MoreFollows = false
}

let private message response =
    Message.SecondaryStationMessage(SecondaryStationMessage.ResponseUserData response)

let private change: ChangePrimaryAddress = {
    OldAddress = response.Address
    NewAddress = PrimaryAddress.PrimAdr(PrimAdr.tryCreate 250uy |> Option.get)
}

let private changeMessage change =
    Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress change)

let private ok =
    function
    | Ok value -> value
    | Error(error: EncodeError) -> failwith error.Message

let private keys onResolve = {
    new IExternalSecurityContextResolver with
        member _.ResolveMode5Context identity =
            onResolve identity
            Some(Mode5ExternalContext(Secret128.TakeOwnership(Array.copy keyBytes)))
}

let private missing = {
    new IExternalSecurityContextResolver with
        member _.ResolveMode5Context _ = None
}

let private run decoder bytes =
    let store =
        InMemorySourceStore(fun data offset -> ByteReaderFactory.Create(data, offset))
        :> ISourceStore

    let root = store.AddRoot "encoder test" (ReadOnlyMemory bytes) false

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
        Sources = store
        Trace = trace
        CreateReader = fun data offset -> ByteReaderFactory.Create(data, offset)
    }

    decoder source context

let private decode resolver bytes =
    Decoder.create resolver (ReadOnlyMemory bytes)

let private access bytes =
    let payload =
        match run DllCodec.decode bytes with
        | Decoded({ Value = Dll.VariableLength frame }, _) ->
            LinkUserDataBytes.bytes frame.LinkUserData.Value
        | actual -> failwith $"Expected variable frame, got {actual}"

    match run TplCodec.decode (payload.ToArray()) with
    | Decoded(tpl, _) ->
        match tpl.Value with
        | Tpl.LongHeader tpl ->
            match tpl.Header.Value with
            | LongHeader.Mode0 header -> header.Acc.Value
            | LongHeader.Mode5 header -> header.Acc.Value
        | actual -> failwith $"Expected long TPL header, got {actual}"
    | actual -> failwith $"Expected TPL header, got {actual}"

let private encoder options resolver =
    Encoder.create options resolver {
        AccessNumber = AccessNumber.create 7uy
    }

let private decoded resolver bytes =
    match decode resolver bytes with
    | Decoded(Message.SecondaryStationMessage(SecondaryStationMessage.ResponseUserData response), _) ->
        response
    | actual -> failwith $"Expected response, got {actual}"

[<Fact>]
let ``mode 5 uses context secondary address and per-call access number for key selection and IV``
    ()
    =
    // EN 13757-7:2018, 9.4.4, Table 48; configuration Table 31.
    let mutable identities = []
    let resolver = keys (fun identity -> identities <- identity :: identities)
    let encode = encoder mode5 resolver
    let bytes = encode (message response) |> ok
    identities |> should equal [ meter ]
    bytes[17..18] |> should equal [| 0xF0uy; 5uy |]
    bytes[19 .. bytes.Length - 3].Length |> should equal 16

    let iv =
        Mode5Iv.create meter 7uy
        |> Result.defaultWith (fun e -> failwith (EncryptionError.value e))

    AesCbCIv.toArray iv
    |> should equal [|
        0xE6uy
        0x1Euy
        0x78uy
        0x56uy
        0x34uy
        0x12uy
        0x3Cuy
        7uy
        7uy
        7uy
        7uy
        7uy
        7uy
        7uy
        7uy
        7uy
    |]

    let actual = decoded resolver bytes
    actual.MeterAddress |> should equal meter
    access bytes |> should equal (AccessNumber.create 7uy)

    actual.UserData
    |> Seq.map _.Value
    |> Seq.toList
    |> should equal [ Value.Int16 42s ]

    let next =
        Encoder.create
            mode5
            resolver
            {
                AccessNumber = AccessNumber.create 8uy
            }
            (message response)
        |> ok

    access next |> should equal (AccessNumber.create 8uy)
    Assert.NotEqual<byte>(bytes[19 .. bytes.Length - 3], next[19 .. next.Length - 3])

[<Fact>]
let ``mode 5 preserves manufacturer bytes instead of absorbing alignment padding`` () =
    // EN 13757-3:2025, Table 6 and 6.6: MDH consumes the remainder of the application data.
    let resolver = keys ignore
    let encode = encoder mode5 resolver
    let original = [| 0x2Fuy; 0uy; 0xFFuy; 0x2Fuy |]

    let bytes =
        encode (
            message {
                response with
                    MfrData = Some(MfrSpecificData.ofBytes (ReadOnlyMemory original))
            }
        )
        |> ok

    let actual = decoded resolver bytes

    actual.MfrData
    |> Option.map (MfrSpecificData.bytes >> _.ToArray())
    |> should equal (Some original)

    actual.UserData
    |> Seq.map _.Value
    |> Seq.toList
    |> should equal [ Value.Int16 42s ]

[<Fact>]
let ``empty records still receive AES check and a full cipher block`` () =
    let resolver = keys ignore
    let bytes = encoder mode5 resolver (message { response with UserData = [] }) |> ok
    bytes.Length |> should equal 37
    (decoded resolver bytes).UserData |> Seq.isEmpty |> should be True

[<Fact>]
let ``missing key returns a protection error without plaintext fallback`` () =
    match encoder mode5 missing (message response) with
    | Error(EncodeError.Protection _) -> ()
    | actual -> failwith $"Expected protection error, got {actual}"

[<Fact>]
let ``plain encoding never resolves a key`` () =
    let resolver = keys (fun _ -> failwith "Plain encoding must not request keys")

    let bytes =
        encoder
            EncodingOptions.Plain
            resolver
            (message {
                response with
                    MeterAddress = otherMeter
            })
        |> ok

    bytes[17..18] |> should equal [| 0uy; 0uy |]
    let actual = decoded missing bytes
    access bytes |> should equal (AccessNumber.create 7uy)
    actual.MeterAddress |> should equal otherMeter

[<Fact>]
let ``mode 5 context secondary address determines response header key selection and IV`` () =
    // EN 13757-7:2018, 7.4/Table 12 and 9.4.4.1/Table 48: header and IV must agree.
    let resolver = keys (fun identity -> identity |> should equal meter)
    let encode = encoder mode5 resolver
    let original = encode (message response) |> ok

    let changedMessage =
        encode (
            message {
                response with
                    MeterAddress = otherMeter
            }
        )
        |> ok

    changedMessage |> should equal original
    (decoded resolver changedMessage).MeterAddress |> should equal meter

    let otherResolver = keys (fun identity -> identity |> should equal otherMeter)
    let otherOptions = EncodingOptions.Mode5 { SecondaryAddress = otherMeter }
    let changedContext = encoder otherOptions otherResolver (message response) |> ok
    changedContext[7..10] |> should equal [| 0x21uy; 0x43uy; 0x65uy; 0x87uy |]

    Assert.NotEqual<byte>(
        original[19 .. original.Length - 3],
        changedContext[19 .. changedContext.Length - 3]
    )

    (decoded otherResolver changedContext).MeterAddress |> should equal otherMeter

[<Fact>]
let ``mode 5 rejects unsupported message combinations explicitly`` () =
    let encode = encoder mode5 (keys (fun _ -> failwith "Reject before key lookup"))

    let alarms: ResponseAlarms = {
        Address = response.Address
        Acd = false
        Dfc = false
        Status = response.Status
        MeterAddress = meter
        Alarms = (parseExactly Alarms.parse [| 0uy |]).Value
    }

    for message in
        [
            Message.SecondaryStationMessage(SecondaryStationMessage.ResponseAlarms alarms)
            message { response with MoreFollows = true }
        ] do
        match encode message with
        | Error(EncodeError.Unsupported _) -> ()
        | actual -> failwith $"Expected unsupported combination, got {actual}"

[<Fact>]
let ``mode 5 enforces the frame limit including AES check and alignment`` () =
    let mutable calls = 0
    let resolver = keys (fun _ -> calls <- calls + 1)
    let encode = encoder mode5 resolver

    let make count =
        message {
            response with
                UserData = []
                MfrData =
                    Some(MfrSpecificData.ofBytes (ReadOnlyMemory(Array.zeroCreate<byte> count)))
        }

    let bytes = encode (make 237) |> ok // MDH + 237 bytes + AES check = 240 bytes.
    bytes.Length |> should equal 261
    calls |> should equal 1

    match encode (make 238) with
    | Error(EncodeError.Unsupported _) -> ()
    | actual -> failwith $"Expected oversized encryption layout, got {actual}"

    calls |> should equal 1

[<Fact>]
let ``change primary address round-trips directly in plain and mode 5 including unsigned addresses``
    ()
    =
    // EN 13757-3:2025, Table 10 and A.3; EN 13757-7:2018, Table 2, 7.4, 9.4.4.
    let resolver = keys (fun identity -> identity |> should equal meter)

    for address in [ 0uy; 1uy; 127uy; 128uy; 250uy ] do
        let newAddress =
            if address = 0uy then
                PrimaryAddress.Unconfigured
            else
                PrimaryAddress.PrimAdr(PrimAdr.tryCreate address |> Option.get)

        let expected = { change with NewAddress = newAddress }

        for options in [ EncodingOptions.Plain; mode5 ] do
            let bytes =
                Encoder.create
                    options
                    resolver
                    {
                        AccessNumber = AccessNumber.create 9uy
                    }
                    (changeMessage expected)
                |> ok

            bytes[4..5] |> should equal [| 0x53uy; 5uy |]

            match options with
            | EncodingOptions.Plain ->
                bytes[6..8] |> should equal [| 0x51uy; 1uy; 0x7Auy |]
                bytes[9] |> should equal address
            | EncodingOptions.Mode5 _ ->
                bytes[6] |> should equal 0x5Buy

                bytes[7..14]
                |> should equal [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 7uy |]

                bytes[15..18] |> should equal [| 9uy; 0uy; 0xF0uy; 5uy |]

                if address = 250uy then
                    // OpenSSL AES-128-CBC, no padding: thirteen 2F bytes followed by 017AFA.
                    bytes[19 .. bytes.Length - 3]
                    |> should equal (Convert.FromHexString "754BC3A839A562A687B3E8C4D682A9A7")

            match decode resolver bytes with
            | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress actual),
                      _) -> actual |> should equal expected
            | actual -> failwith $"Expected address change, got {actual}"

[<Fact>]
let ``address change mode 5 takes access and identity from context rather than link address`` () =
    let resolver = keys ignore
    let encode = Encoder.create mode5 resolver

    let context = {
        AccessNumber = AccessNumber.create 9uy
    }

    let original = encode context (changeMessage change) |> ok

    let next =
        encode
            {
                AccessNumber = AccessNumber.create 10uy
            }
            (changeMessage change)
        |> ok

    access next |> should equal (AccessNumber.create 10uy)
    Assert.NotEqual<byte>(original[19 .. original.Length - 3], next[19 .. next.Length - 3])

    let otherLink =
        encode
            context
            (changeMessage {
                change with
                    OldAddress = AField.Configured(PrimAdr.tryCreate 6uy |> Option.get)
            })
        |> ok

    otherLink[19 .. otherLink.Length - 3]
    |> should equal original[19 .. original.Length - 3]

    let otherResolver = keys (fun identity -> identity |> should equal otherMeter)

    let otherBytes =
        Encoder.create
            (EncodingOptions.Mode5 { SecondaryAddress = otherMeter })
            otherResolver
            context
            (changeMessage change)
        |> ok

    otherBytes[7..10] |> should equal [| 0x21uy; 0x43uy; 0x65uy; 0x87uy |]

    Assert.NotEqual<byte>(
        original[19 .. original.Length - 3],
        otherBytes[19 .. otherBytes.Length - 3]
    )

    match decode otherResolver otherBytes with
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress actual), _) ->
        actual |> should equal change
    | actual -> failwith $"Expected address change using context identity, got {actual}"

[<Fact>]
let ``address change mode 5 requires a key and rejects a wrong key`` () =
    match encoder mode5 missing (changeMessage change) with
    | Error(EncodeError.Protection _) -> ()
    | actual -> failwith $"Expected missing key error, got {actual}"

    let bytes = encoder mode5 (keys ignore) (changeMessage change) |> ok

    let wrong = {
        new IExternalSecurityContextResolver with
            member _.ResolveMode5Context _ =
                Some(Mode5ExternalContext(Secret128.TakeOwnership(Array.zeroCreate 16)))
    }

    for resolver in [ missing; wrong ] do
        match decode resolver bytes with
        | DecodeFailed(EncryptionFailed _, _) -> ()
        | actual -> failwith $"Expected protection failure, got {actual}"

[<Fact>]
let ``plain TPL responses and alarms take access number from each encoding call`` () =
    let encode = Encoder.create EncodingOptions.Plain missing

    let alarms: ResponseAlarms = {
        Address = response.Address
        Acd = false
        Dfc = false
        Status = response.Status
        MeterAddress = meter
        Alarms = (parseExactly Alarms.parse [| 0uy |]).Value
    }

    for number in [ 0uy; 127uy; 255uy ] do
        let context = {
            AccessNumber = AccessNumber.create number
        }

        let dataBytes = encode context (message response) |> ok
        access dataBytes |> should equal context.AccessNumber

        let alarmBytes =
            encode
                context
                (Message.SecondaryStationMessage(SecondaryStationMessage.ResponseAlarms alarms))
            |> ok

        alarmBytes[15] |> should equal number

[<Fact>]
let ``mode 5 keeps link services in their defined format without requesting keys`` () =
    let resolver = keys (fun _ -> failwith "Link services have no TPL protection")

    let services = [
        PrimaryStationMessage.LinkLayerReset { Address = response.Address }
        PrimaryStationMessage.SelectionOfDevice {
            Selection = DeviceSelection.AllDevices
        }
        PrimaryStationMessage.RequestUserData {
            Address = AField.Diagnosis
            Fcb = true
        }
        PrimaryStationMessage.RequestAlarms {
            Address = AField.Diagnosis
            Fcb = false
        }
    ]

    for service in services do
        let message = Message.PrimaryStationMessage service
        let plain = encoder EncodingOptions.Plain resolver message |> ok
        encoder mode5 resolver message |> ok |> should equal plain

[<Fact>]
let ``address change mapping validates wired partner status and SND-UD FCV`` () =
    let resolver = keys ignore
    let original = encoder mode5 resolver (changeMessage change) |> ok

    let modify index value =
        let bytes = Array.copy original
        bytes[index] <- value

        bytes[bytes.Length - 2] <-
            bytes[4 .. bytes.Length - 3]
            |> Array.fold (fun sum value -> (sum + int value) % 256) 0
            |> byte

        bytes

    for bytes in [ modify 16 1uy; modify 16 0x80uy ] do
        match decode resolver bytes with
        | DecodeFailed(ValidationFailed _, _) -> ()
        | actual -> failwith $"Expected invalid wired address change, got {actual}"
    // FCV=0 is a separate SND-UD2 service; the device rejects it without changing the address.
    match decode resolver (modify 4 0x43uy) with
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.SendUserData2 _), _) -> ()
    | actual -> failwith $"Expected a separate SND-UD2 service, got {actual}"
    // Table 17 allows zero for either RSSI or link margin.
    match decode resolver (modify 16 0x40uy) with
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress actual), _) ->
        actual |> should equal change
    | actual -> failwith $"Expected address change without link-margin information, got {actual}"

[<Fact>]
let ``address change mapping rejects reserved addresses and other record actions`` () =
    let original = encoder EncodingOptions.Plain missing (changeMessage change) |> ok

    let frame (payload: byte[]) =
        let data = Array.append original[4..5] payload
        let length = byte data.Length
        let checksum = data |> Array.sumBy int |> fun sum -> byte (sum % 256)
        Array.concat [ [| 0x68uy; length; length; 0x68uy |]; data; [| checksum; 0x16uy |] ]

    for payload in
        [
            [| 0x51uy; 1uy; 0x7Auy; 251uy |]
            [| 0x51uy; 1uy; 0xFAuy; 1uy; 5uy |] // ADD instead of SET
            [| 0x51uy; 1uy; 0x7Auy; 1uy; 1uy; 0x7Auy; 2uy |] // two address records
        ] do
        match decode missing (frame payload) with
        | DecodeFailed(ValidationFailed _, _) -> ()
        | actual -> failwith $"Expected invalid address SET, got {actual}"

    match decode missing (frame [| 0x51uy; 1uy; 0xFAuy; 0uy; 250uy |]) with
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress actual), _) ->
        actual |> should equal change
    | actual -> failwith $"Expected explicit SET action, got {actual}"

[<Fact>]
let ``AES CBC encryption matches NIST and leaves the caller key unchanged`` () =
    let original = Convert.FromHexString "2b7e151628aed2a6abf7158809cf4f3c"
    let key = Secret128.TakeOwnership(Array.copy original)

    let iv =
        AesCbCIv.create (Convert.FromHexString "000102030405060708090a0b0c0d0e0f")
        |> Result.defaultWith (fun e -> failwith (EncryptionError.value e))

    let plain = ReadOnlyMemory(Convert.FromHexString "6bc1bee22e409f96e93d7e117393172a")

    for _ in 1..2 do
        let encrypted =
            AesCbc.encrypt key iv plain
            |> Result.defaultWith (fun e -> failwith (EncryptionError.value e))

        encrypted.ToArray()
        |> should equal (Convert.FromHexString "7649abac8119b246cee98e9b12e9197d")

        key.ToArray |> should equal original

    match AesCbc.encrypt key iv (ReadOnlyMemory [| 0uy |]) with
    | Error _ -> ()
    | _ -> failwith "Unaligned plaintext must fail"
