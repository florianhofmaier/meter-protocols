module Metering.Mbus.Codec.Tests.DecoderTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Records

let private noKeys = {
    new IExternalSecurityContextResolver with
        member _.ResolveMode5Context _ =
            failwith "Plain frames must not resolve keys"
}

// EN 13757-2, Annex F.2, p. 37: bounded REQ-UD2 example.
let private request = [| 0x10uy; 0x5Buy; 1uy; 0x5Cuy; 0x16uy |]

let private assertRequest =
    function
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.RequestUserData message), []) ->
        message.Fcb |> should be False

        RequestUserData.write message
        |> Metering.Mbus.Protocol.Utility.Writer.runFt12Writer
        |> Result.defaultWith (fun error -> failwith error.Msg)
        |> should equal request
    | actual -> failwith $"Expected request without notices, got {actual}"

[<Fact>]
let ``decodes a message directly from a sliced memory buffer`` () =
    let buffer = Array.concat [ [| 0xFFuy |]; request; [| 0xFFuy |] ]

    Decoder.create noKeys (ReadOnlyMemory(buffer, 1, request.Length))
    |> assertRequest

[<Fact>]
let ``created decoder can be reused after successful and failed calls`` () =
    let decode = Decoder.create noKeys

    for _ in 1..2 do
        decode (ReadOnlyMemory request) |> assertRequest

        match decode (ReadOnlyMemory request[..2]) with
        | DecodeFailed(ParseFailed _, _) -> ()
        | actual -> failwith $"Expected parse failure, got {actual}"

[<Fact>]
let ``empty truncated and trailing inputs retain parse failures`` () =
    for bytes in [ [||]; request[..2]; Array.append request [| 0uy |] ] do
        match Decoder.create noKeys (ReadOnlyMemory bytes) with
        | DecodeFailed(ParseFailed _, _) -> ()
        | actual -> failwith $"Expected parse failure, got {actual}"

[<Fact>]
let ``corrupt frame retains validation failure`` () =
    let bytes = Array.copy request
    bytes[3] <- 0uy

    match Decoder.create noKeys (ReadOnlyMemory bytes) with
    | DecodeFailed(ValidationFailed _, _) -> ()
    | actual -> failwith $"Expected validation failure, got {actual}"

[<Fact>]
let ``existing control field warning is retained on success and failure`` () =
    for control, checksum, valid in [ 0xDBuy, 0xDCuy, true; 0xCBuy, 0xCCuy, false ] do
        let bytes = [| 0x10uy; control; 1uy; checksum; 0x16uy |]

        let notices =
            match Decoder.create noKeys (ReadOnlyMemory bytes), valid with
            | Decoded(_, notices), true -> notices
            | DecodeFailed(ValidationFailed _, notices), false -> notices
            | actual -> failwith $"Unexpected decode result: {actual}"

        match notices with
        | [ Warning issue ] -> issue.Message |> should equal "Reserved bit must be 0"
        | actual -> failwith $"Expected control field warning, got {actual}"

let private protectedResponse () =
    // EN 13757-7, 9.4.4.1, Table 48, p. 62 and 9.4.4.2, p. 63.
    let key = Array.init 16 byte
    let identity = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0x2Duy; 0x2Cuy; 1uy; 7uy |]

    let iv =
        Array.concat [ identity[4..5]; identity[..3]; identity[6..7]; Array.create 8 8uy ]

    use aes = System.Security.Cryptography.Aes.Create()
    aes.Key <- key

    let cipher =
        aes.EncryptCbc(Array.create 16 0x2Fuy, iv, System.Security.Cryptography.PaddingMode.None)

    let data =
        Array.concat [ [| 8uy; 1uy; 0x72uy |]; identity; [| 8uy; 0uy; 0x10uy; 5uy |]; cipher ]

    let length = byte data.Length
    let checksum = byte ((data |> Array.sumBy int) &&& 255)

    let frame =
        Array.concat [ [| 0x68uy; length; length; 0x68uy |]; data; [| checksum; 0x16uy |] ]

    key, ReadOnlyMemory frame

[<Fact>]
let ``uses supplied resolver and creates sources needed for decryption`` () =
    let key, bytes = protectedResponse ()
    let mutable calls = 0

    let resolver = {
        new IExternalSecurityContextResolver with
            member _.ResolveMode5Context identity =
                calls <- calls + 1
                Manufacturer.value identity.Mfr |> should equal 0x2C2Dus
                Some(Mode5ExternalContext(Secret128.TakeOwnership(Array.copy key)))
    }

    let decode = Decoder.create resolver
    calls |> should equal 0

    for _ in 1..2 do
        match decode bytes with
        | Decoded(Message.SecondaryStationMessage(SecondaryStationMessage.ResponseUserData message),
                  []) -> message.UserData |> Seq.isEmpty |> should be True
        | actual -> failwith $"Expected decrypted response, got {actual}"

    calls |> should equal 2

[<Fact>]
let ``missing key retains encryption failure`` () =
    let _, bytes = protectedResponse ()

    let resolver = {
        new IExternalSecurityContextResolver with
            member _.ResolveMode5Context _ = None
    }

    match Decoder.create resolver bytes with
    | DecodeFailed(EncryptionFailed issue, _) ->
        issue.Message |> should startWith "No external security context"
    | actual -> failwith $"Expected encryption failure, got {actual}"

[<Fact>]
let ``user data encodes and validates a complete data set larger than one telegram`` () =
    // EN 13757-3:2025, 6.2: the record sequence is independent of telegram framing.
    let record = [| 0x04uy; 0x13uy; 0xD2uy; 0x04uy; 0uy; 0uy |]
    let bytes = Array.init 100 (fun _ -> record) |> Array.concat

    match UserData.decode (ReadOnlyMemory bytes) with
    | Decoded(data, []) ->
        let records = RspUdData.records data

        records.Length |> should equal 100

        match UserData.encode records with
        | Ok encoded -> encoded |> should equal bytes
        | Error error -> failwith error.Msg
    | actual -> failwith $"Expected a complete user-data set, got {actual}"

[<Fact>]
let ``user data encoding stops at the first invalid record`` () =
    let bytes = [| 0x04uy; 0x13uy; 0uy; 0uy; 0uy; 0uy |]

    let record =
        match UserData.decode (ReadOnlyMemory bytes) with
        | Decoded(data, _) -> RspUdData.records data |> List.head
        | actual -> failwith $"Expected valid record, got {actual}"

    let records =
        seq {
            yield {
                record with
                    Value = Value.Text(String.replicate 192 "x")
            }

            failwith "The remainder must not be enumerated after a writing failure"
        }

    match UserData.encode records with
    | Error error -> Assert.Contains("191", error.Msg)
    | Ok _ -> failwith "Expected an invalid text value to fail"

module ApplicationSelection = Metering.Mbus.Protocol.Frames.ApplicationLayer.ApplicationResetOrSelect

let private commandFrame body =
    let body = Array.ofList body
    let length = byte body.Length
    let checksum = byte ((body |> Array.sumBy int) &&& 255)
    Array.concat [ [| 0x68uy; length; length; 0x68uy |]; body; [| checksum; 0x16uy |] ]

[<Fact>]
let ``application commands accept their none short and long TPL headers`` () =
    let identity = [ 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 7uy ]

    for ci, header in
        [
            0x50uy, []
            0x57uy, [ 0uy; 0uy; 0uy; 0uy ]
            0x53uy, identity @ [ 0uy; 0uy; 0uy; 0uy ]
        ] do
        let bytes = commandFrame ([ 0x53uy; 5uy; ci ] @ header @ [ 1uy ])

        match Decoder.create noKeys (ReadOnlyMemory bytes) with
        | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.ApplicationResetOrSelect command),
                  []) ->
            let application, block = ApplicationSelection.selection command.Selection
            Assert.Equal(0, application)
            Assert.Equal(1, block)
        | actual -> failwith $"Expected application selection, got {actual}"

    for ci, header in
        [
            0x54uy, []
            0x56uy, [ 0uy; 0uy; 0uy; 0uy ]
            0x55uy, identity @ [ 0uy; 0uy; 0uy; 0uy ]
        ] do
        for control in [ 0x53uy; 0x43uy ] do
            match
                Decoder.create noKeys (ReadOnlyMemory(commandFrame ([ control; 5uy; ci ] @ header)))
            with
            | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.RequestSelectedApplication command),
                      []) -> Assert.Equal((control = 0x43uy), command.Immediate)
            | actual -> failwith $"Expected selected-application request, got {actual}"

[<Fact>]
let ``selected application responses preserve opaque data with and without long headers`` () =
    let identity = [ 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 7uy ]
    let payload = [ 0uy; 0uy; 0uy; 4uy; 0x13uy; 1uy; 0uy; 0uy; 0uy ]

    for ci, header in [ 0x66uy, []; 0x68uy, identity @ [ 17uy; 0uy; 0uy; 0uy ] ] do
        let bytes = commandFrame ([ 8uy; 5uy; ci ] @ header @ payload)

        match Decoder.create noKeys (ReadOnlyMemory bytes) with
        | Decoded(Message.SecondaryStationMessage(SecondaryStationMessage.ResponseSelectedApplication response) as message,
                  []) ->
            Assert.Equal<byte>(payload, response.Data.ToArray())

            match
                Encoder.create
                    EncodingOptions.Plain
                    noKeys
                    {
                        AccessNumber =
                            Metering.Mbus.Protocol.Frames.TransportLayer.AccessNumber.create 17uy
                    }
                    message
            with
            | Ok encoded -> Assert.Equal<byte>(bytes, encoded)
            | Error error -> failwith error.Message
        | actual -> failwith $"Expected selected-application response, got {actual}"

[<Fact>]
let ``unsupported SND UD2 fallback never accepts a corrupt DLL`` () =
    let bytes = commandFrame [ 0x43uy; 5uy; 0xFFuy ]

    match Decoder.create noKeys (ReadOnlyMemory bytes) with
    | Decoded(Message.PrimaryStationMessage(PrimaryStationMessage.SendUserData2 _), _) -> ()
    | actual -> failwith $"Expected unsupported send service, got {actual}"

    bytes[bytes.Length - 2] <- 0uy

    match Decoder.create noKeys (ReadOnlyMemory bytes) with
    | DecodeFailed _ -> ()
    | _ -> failwith "Corrupt DLL must not reach the device"
