module Metering.Mbus.LayerCodecs.Tests.LayerCodecTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages

type private NoSecurityContext() =
    interface IExternalSecurityContextResolver with
        member _.ResolveMode5Context(_identity: DeviceIdentification) = None

let private validationValue =
    function
    | Passed (value, _) -> value
    | Failed (failures, _) -> failwith $"Expected valid layer, got {Failures.toList failures}"


let private contextFor bytes =
    let store =
        InMemorySourceStore(fun value offset -> ByteReaderFactory.Create(value, offset))
        :> ISourceStore

    let root = store.AddRoot "layer codec test" (ReadOnlyMemory<byte> bytes) false
    let mutable nextId = 0
    let trace =
        { new IFieldTracer with
            member _.SourceCreated _ = failwith "Layer codecs must not create derived sources"
            member _.BeginField(_, _) =
                let id = FieldId.create nextId
                nextId <- nextId + 1
                id
            member _.EndField(_, _) = ()
            member _.FailField(_, _) = () }

    let source = {
        Id = FieldId.create -1
        Span = { Source = root.Id; Offset = 0; Length = bytes.Length }
        Value = ReadOnlyMemory<byte> bytes
    }
    source, {
        CreateReader = fun value offset -> ByteReaderFactory.Create(value, offset)
        Sources = store
        Trace = trace
    }

let private parsed parse bytes =
    let source, context = contextFor bytes
    match Core.parse parse source context with
    | Decoded (raw, _) -> raw
    | actual -> failwith $"Expected parsed structure, got {actual}"

// Opaque bytes are deliberately not a supported higher-layer message.
let private variableFrame =
    [| 0x68uy; 0x04uy; 0x04uy; 0x68uy; 0x08uy; 0x01uy; 0xFFuy; 0xFFuy; 0x07uy; 0x16uy |]

[<Fact>]
let ``DLL decodes the supported FT 1.2 alternatives without higher-layer parsing`` () =
    // Fixed-length example: EN 13757-2, Annex F.2, p. 37.
    let cases = [ [| 0xE5uy |]; [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]; variableFrame ]
    cases |> List.iteri (fun index bytes ->
        let source, context = contextFor bytes
        match DllCodec.decode source context with
        | Decoded (field, _) ->
            field.Span |> should equal source.Span
            match index, field.Value with
            | 0, Dll.SingleCharacter
            | 1, Dll.FixedLength _ -> ()
            | 2, Dll.VariableLength frame ->
                frame.LinkUserData.Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]
                frame.LinkUserData.Span.Offset |> should equal 6
                frame.LinkUserData.Span.Length |> should equal 2
            | actual -> failwith $"Unexpected DLL variant: {actual}"
        | actual -> failwith $"Expected successful DLL decode, got {actual}")

[<Fact>]
let ``DLL validation retains frame and payload provenance`` () =
    let raw = parsed DllCodec.parse variableFrame
    let valid = DllCodec.fromRaw raw |> validationValue
    valid.Id |> should equal raw.Id
    valid.Span |> should equal raw.Span
    match raw.Value, valid.Value with
    | DllRaw.VariableLength before, Dll.VariableLength after ->
        after.CField.Id |> should equal before.UserData.Value.CField.Id
        after.AField.Span |> should equal before.UserData.Value.AField.Span
        after.LinkUserData |> should equal before.UserData.Value.LinkUserData
    | actual -> failwith $"Expected variable-length alternatives, got {actual}"

[<Fact>]
let ``DLL collects independent repeated-length checksum and end errors`` () =
    let bytes = Array.copy variableFrame
    bytes[2] <- 5uy
    bytes[8] <- 0uy
    bytes[9] <- 0uy
    let raw = parsed DllCodec.parse bytes
    match raw.Value, DllCodec.fromRaw raw with
    | DllRaw.VariableLength frame, Failed (failures, _) ->
        let issues = Failures.toList failures
        issues.Length |> should equal 3
        let ids = issues |> List.map _.FieldId |> Set.ofList
        ids |> should equal (Set.ofList [ frame.Length2.Id; frame.Crc.Id; frame.End.Id ])
    | actual -> failwith $"Expected all three local failures, got {actual}"

[<Fact>]
let ``fixed-length DLL collects failures for both unsupported C and A alternatives`` () =
    // QS-001: both fields are outside the alternatives supported by their types.
    let raw = parsed DllCodec.parse [| 0x10uy; 0x41uy; 0xFCuy; 0x3Duy; 0x16uy |]
    match raw.Value, DllCodec.fromRaw raw with
    | DllRaw.FixedLength frame, Failed (failures, _) ->
        let ids = failures |> Failures.toList |> List.map _.FieldId |> Set.ofList
        ids |> should equal (Set.ofList [ frame.UserData.Value.CField.Id; frame.UserData.Value.AField.Id ])
    | actual -> failwith $"Expected both independent field failures, got {actual}"

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``DLL decode rejects truncated or trailing input`` trailing =
    let bytes =
        if trailing then Array.append variableFrame [| 0uy |]
        else variableFrame[..variableFrame.Length - 2]
    let source, context = contextFor bytes
    match DllCodec.decode source context with
    | DecodeFailed (ParseFailed _, _) -> ()
    | actual -> failwith $"Expected structural failure, got {actual}"

let private longHeader configuration =
    [|
        0x72uy
        0x78uy; 0x56uy; 0x34uy; 0x12uy
        0x2Duy; 0x2Cuy
        0x01uy; 0x07uy
        0x08uy; 0x00uy
        byte configuration; byte (configuration >>> 8)
    |]

[<Theory>]
[<InlineData(0, 1)>]
[<InlineData(1, 5)>]
[<InlineData(2, 13)>]
let ``TPL preserves opaque application bytes for every supported header shape`` shape payloadOffset =
    // EN 13757-7, 7.2-7.4, Tables 11-12: 0/4/12 bytes after CI.
    let header =
        match shape with
        | 0 -> [| 0x51uy |]
        | 1 -> [| 0x7Auy; 0x08uy; 0x00uy; 0x00uy; 0x00uy |]
        | _ -> longHeader 0us
    let bytes = Array.append header [| 0xFFuy; 0xFFuy |]
    let raw = parsed TplCodec.parse bytes
    let valid = TplCodec.fromRaw raw |> validationValue
    let payload =
        match shape, valid.Value with
        | 0, Tpl.NoneHeader tpl -> tpl.AplData
        | 1, Tpl.ShortHeader tpl -> tpl.AplData
        | 2, Tpl.LongHeader tpl -> tpl.AplData
        | actual -> failwith $"Unexpected TPL variant: {actual}"
    valid.Id |> should equal raw.Id
    valid.Span |> should equal raw.Span
    payload |> should equal (TplRaw.aplData raw.Value)
    payload.Span.Offset |> should equal payloadOffset
    payload.Span.Length |> should equal 2
    payload.Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]

[<Fact>]
let ``TPL mode five decode retains ciphertext without requiring a key or decrypting`` () =
    // Mode 5 configuration, one encrypted block; layout verification belongs to Protection.
    let bytes = Array.append (longHeader 0x0510us) [| 0xFFuy |]
    let source, context = contextFor bytes
    match TplCodec.decode source context with
    | Decoded ({ Value = Tpl.LongHeader { Header = { Value = LongHeader.Mode5 _ }; AplData = payload } }, _) ->
        payload.Value.ToArray() |> should equal [| 0xFFuy |]
    | actual -> failwith $"Expected TPL metadata and opaque ciphertext, got {actual}"

[<Fact>]
let ``TPL reports a truncated present header as a parse error`` () =
    let source, context = contextFor [| 0x7Auy; 0x08uy |]
    match TplCodec.decode source context with
    | DecodeFailed (ParseFailed _, _) -> ()
    | actual -> failwith $"Expected structural failure, got {actual}"

[<Fact>]
let ``wired decoder stops after invalid DLL before parsing malformed higher-layer bytes`` () =
    let bytes = Array.copy variableFrame
    bytes[8] <- 0uy
    let source, context = contextFor bytes
    let resolver = NoSecurityContext()
    match MbusDecoder.decodeFrame resolver source context with
    | DecodeFailed (ValidationFailed failures, _) ->
        // A higher-layer parse would fail on CI 0xFF. Only the DLL checksum is reported.
        let issues = Failures.toList failures
        issues.Length |> should equal 1
        issues.Head.Message |> should startWith "CRC mismatch"
    | actual -> failwith $"Expected DLL validation failure before higher-layer parsing, got {actual}"

[<Fact>]
let ``wired decoder retains DLL bytes alongside separately decoded higher layers`` () =
    // EN 13757-3, 7.1, p. 39: CI 0x50 without parameters is application reset.
    let bytes = [| 0x68uy; 0x03uy; 0x03uy; 0x68uy; 0x53uy; 0x01uy; 0x50uy; 0xA4uy; 0x16uy |]
    let source, context = contextFor bytes
    let resolver = NoSecurityContext()
    match MbusDecoder.decodeFrame resolver source context with
    | Decoded ({ Value = WiredMbusFrame.VariableLength (dll, LinkUserData.MbusProtocol (MbusProtocolUserData.Complete higher)) }, _) ->
        dll.Span |> should equal source.Span
        dll.Value.LinkUserData.Value.ToArray() |> should equal [| 0x50uy |]
        dll.Value.LinkUserData.Span.Offset |> should equal 6
        higher.Span |> should equal dll.Value.LinkUserData.Span
        match higher.Value.Apl.Value with
        | Apl.ApplicationResetOrSelect ApplicationReset -> ()
        | actual -> failwith $"Expected decoded application reset, got {actual}"
    | actual -> failwith $"Expected separate DLL and higher-layer results, got {actual}"

[<Fact>]
let ``orchestrator returns a request with the parsed frame provenance`` () =
    // EN 13757-2, Annex F.2, p. 37.
    let bytes = [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]
    let raw = parsed DllCodec.parse bytes
    let source, context = contextFor bytes
    match MbusDecoder.decode (NoSecurityContext()) source context with
    | Decoded ({ Value = Message.PrimaryStationMessage (PrimaryStationMessage.RequestUserData request) } as message, _) ->
        message.Id |> should equal raw.Id
        message.Span |> should equal raw.Span
        request.Fcb |> should be False
        match request.Address with
        | RequestUserDataAddress.Configured address -> PrimAdr.value address |> should equal 1uy
        | actual -> failwith $"Expected primary address 1, got {actual}"
    | actual -> failwith $"Expected request user data message, got {actual}"

[<Fact>]
let ``orchestrator returns response user data after higher-layer processing`` () =
    // EN 13757-3, 6.3.3, Table 6: 0x2F is an idle filler.
    let userData = Array.concat [ [| 0x08uy; 0x01uy |]; longHeader 0us; [| 0x2Fuy |] ]
    let length = byte userData.Length
    let checksum = userData |> Array.fold (+) 0uy
    let bytes = Array.concat [ [| 0x68uy; length; length; 0x68uy |]; userData; [| checksum; 0x16uy |] ]
    let source, context = contextFor bytes
    match MbusDecoder.decode (NoSecurityContext()) source context with
    | Decoded ({ Value = Message.SecondaryStationMessage (SecondaryStationMessage.ResponseUserData response) } as message, _) ->
        message.Span |> should equal source.Span
        response.UserData |> Seq.isEmpty |> should be True
        response.AccessNumber |> AccessNumber.value |> should equal 8uy
        response.MoreFollows |> should be False
    | actual -> failwith $"Expected response user data message, got {actual}"

[<Fact>]
let ``orchestrator rejects a valid frame without an implemented message mapping`` () =
    // Application reset is parsable, but currently has no mapping in Message.fromFrame.
    let bytes = [| 0x68uy; 0x03uy; 0x03uy; 0x68uy; 0x53uy; 0x01uy; 0x50uy; 0xA4uy; 0x16uy |]
    let raw = parsed DllCodec.parse bytes
    let source, context = contextFor bytes
    match MbusDecoder.decode (NoSecurityContext()) source context with
    | DecodeFailed (ValidationFailed failures, _) ->
        let issues = Failures.toList failures
        issues.Length |> should equal 1
        issues.Head.FieldId |> should equal raw.Id
        issues.Head.Message |> should equal "frame is not a supported M-Bus message"
    | actual -> failwith $"Expected unsupported message mapping failure, got {actual}"

let private wired payload =
    let userData = Array.append [| 0x08uy; 0x01uy |] payload
    let length = byte userData.Length
    let checksum = userData |> Array.fold (+) 0uy
    Array.concat [ [| 0x68uy; length; length; 0x68uy |]; userData; [| checksum; 0x16uy |] ]

let private observe context =
    let names = ResizeArray<string>()
    let sources = ResizeArray<SourceInfo>()
    let trace =
        { new IFieldTracer with
            member _.SourceCreated source = sources.Add source
            member _.BeginField(name, span) =
                names.Add name
                context.Trace.BeginField(name, span)
            member _.EndField(id, span) = context.Trace.EndField(id, span)
            member _.FailField(id, error) = context.Trace.FailField(id, error) }
    { context with Trace = trace }, names, sources

type private CountingKeys(key: byte[] option) =
    let mutable calls = 0
    member _.Calls = calls
    interface IExternalSecurityContextResolver with
        member _.ResolveMode5Context _ =
            calls <- calls + 1
            key |> Option.map (fun bytes ->
                Mode5ExternalContext(Metering.Common.Security.Cryptography.Secret128.TakeOwnership(Array.copy bytes)))

[<Fact>]
let ``final Message codec only validates the assembled frame`` () =
    let source, context = contextFor [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]
    let frame =
        match MbusDecoder.decodeFrame (NoSecurityContext()) source context with
        | Decoded (frame, _) -> frame
        | actual -> failwith $"Expected assembled frame, got {actual}"
    let forbidden = {
        context with
            CreateReader = fun _ _ -> failwith "Message must not parse"
            Trace = { new IFieldTracer with
                member _.SourceCreated _ = failwith "Message must not transform"
                member _.BeginField(_, _) = failwith "Message must not parse"
                member _.EndField(_, _) = failwith "Message must not parse"
                member _.FailField(_, _) = failwith "Message must not parse" }
    }
    match Message.decode frame forbidden with
    | Decoded (message, _) ->
        message.Id |> should equal frame.Id
        message.Span |> should equal frame.Span
    | actual -> failwith $"Expected final semantic mapping, got {actual}"

[<Fact>]
let ``orchestrator stops on invalid TPL before key lookup or APL parsing`` () =
    let source, context = contextFor (wired (Array.append (longHeader 0x0514us) (Array.zeroCreate 16)))
    let context, names, sources = observe context
    let keys = CountingKeys(Some (Array.zeroCreate 16))
    match MbusDecoder.decode keys source context with
    | DecodeFailed (ValidationFailed _, _) -> ()
    | actual -> failwith $"Expected TPL validation failure, got {actual}"
    keys.Calls |> should equal 0
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

[<Theory>]
[<InlineData(0x0510, 1)>]
[<InlineData(0x05F0, 17)>]
let ``invalid encryption layout fails before key lookup`` configuration payloadLength =
    let source, context = contextFor (wired (Array.append (longHeader (uint16 configuration)) (Array.zeroCreate payloadLength)))
    let context, names, sources = observe context
    let keys = CountingKeys(None)
    match MbusDecoder.decode keys source context with
    | DecodeFailed (EncryptionFailed _, _) -> ()
    | actual -> failwith $"Expected invalid encryption layout, got {actual}"
    keys.Calls |> should equal 0
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

[<Fact>]
let ``missing key is a Protection failure and prevents APL parsing`` () =
    let source, context = contextFor (wired (Array.append (longHeader 0x0510us) (Array.zeroCreate 16)))
    let context, names, sources = observe context
    let keys = CountingKeys(None)
    match MbusDecoder.decode keys source context with
    | DecodeFailed (EncryptionFailed issue, _) -> issue.Message |> should startWith "No external security context"
    | actual -> failwith $"Expected missing key, got {actual}"
    keys.Calls |> should equal 1
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

let private encryptMode5 key plaintext =
    // Independent fixture: EN 13757-7, 9.4.4.1, Table 48; address from longHeader.
    let iv = [| 0x2Duy; 0x2Cuy; 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0x01uy; 0x07uy
                0x08uy; 0x08uy; 0x08uy; 0x08uy; 0x08uy; 0x08uy; 0x08uy; 0x08uy |]
    use aes = System.Security.Cryptography.Aes.Create()
    aes.Mode <- System.Security.Cryptography.CipherMode.CBC
    aes.Padding <- System.Security.Cryptography.PaddingMode.None
    use encryptor = aes.CreateEncryptor(key, iv)
    encryptor.TransformFinalBlock(plaintext, 0, plaintext.Length)

[<Theory>]
[<InlineData(0x0510, true)>]
[<InlineData(0x05F0, false)>]
let ``Protection preserves clear suffix and APL is parsed from the derived source`` configuration suffix =
    // EN 13757-7, 7.6 and 9.4.4: partial prefix or all N bits set; verified 2F 2F.
    let key = Array.init 16 byte
    let plain = Array.create 16 0x2Fuy
    let clear = if suffix then [| 0x2Fuy |] else [||]
    let payload = Array.append (encryptMode5 key plain) clear
    let source, context = contextFor (wired (Array.append (longHeader (uint16 configuration)) payload))
    let context, _, sources = observe context
    let keys = CountingKeys(Some key)
    match MbusDecoder.decodeFrame keys source context with
    | Decoded ({ Value = WiredMbusFrame.VariableLength (dll, LinkUserData.MbusProtocol (MbusProtocolUserData.Complete higher)) }, _) ->
        keys.Calls |> should equal 1
        sources.Count |> should equal 1
        let derived = sources[0]
        derived.Sensitive |> should be True
        let encrypted = Tpl.aplData higher.Value.Tpl.Value
        derived.Origin |> should equal (Some encrypted.Span)
        encrypted.Span.Offset |> should equal 19
        encrypted.Span.Source |> should equal dll.Span.Source
        higher.Value.Apl.Span.Source |> should equal derived.Id
        (context.Sources.CreateReader derived.Id).Buffer.ToArray() |> should equal (Array.append plain clear)
        match higher.Value.Apl.Value with
        | Apl.RspUdData response -> RspUdData.records response |> Seq.isEmpty |> should be True
        | actual -> failwith $"Expected response records, got {actual}"
    | actual -> failwith $"Expected protected response, got {actual}"

[<Fact>]
let ``failed decryption verification prevents APL and derived source creation`` () =
    let key = Array.zeroCreate 16
    let payload = encryptMode5 key (Array.zeroCreate 16)
    let source, context = contextFor (wired (Array.append (longHeader 0x0510us) payload))
    let context, names, sources = observe context
    match MbusDecoder.decode (CountingKeys(Some key)) source context with
    | DecodeFailed (EncryptionFailed issue, _) -> issue.Message |> should equal "Mode 5 decryption verification failed."
    | actual -> failwith $"Expected verification failure, got {actual}"
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

[<Fact>]
let ``zero encrypted blocks preserve the original source without key lookup`` () =
    let source, context = contextFor (wired (Array.append (longHeader 0x0500us) [| 0x2Fuy |]))
    let keys = CountingKeys(None)
    match MbusDecoder.decode keys source context with
    | Decoded (_, _) -> keys.Calls |> should equal 0
    | actual -> failwith $"Expected clear payload without key, got {actual}"

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``orchestrator represents optional ELL presence and retains the following layer`` present =
    let tpl = [| 0x50uy |]
    let payload = if present then Array.append [| 0x8Cuy; 0x00uy; 0x08uy |] tpl else tpl
    let source, context = contextFor (wired payload)
    match MbusDecoder.decodeFrame (NoSecurityContext()) source context with
    | Decoded ({ Value = WiredMbusFrame.VariableLength (_, LinkUserData.MbusProtocol (MbusProtocolUserData.Complete higher)) }, _) ->
        higher.Value.Ell.IsSome |> should equal present
        higher.Value.Tpl.Span.Offset |> should equal (if present then 9 else 6)
    | actual -> failwith $"Expected optional ELL, got {actual}"

[<Theory>]
[<InlineData(0x8C)>]
[<InlineData(0xFF)>]
let ``malformed present ELL or unknown CI cannot become absent`` ci =
    let source, context = contextFor (wired [| byte ci |])
    let context, names, _ = observe context
    match MbusDecoder.decodeFrame (NoSecurityContext()) source context with
    | DecodeFailed (ParseFailed _, _) -> ()
    | actual -> failwith $"Expected parse error, got {actual}"
    names.Contains "TPL" |> should be False

[<Fact>]
let ``ELL codec retains opaque following bytes without calling TPL`` () =
    let source, context = contextFor [| 0x8Cuy; 0x00uy; 0x08uy; 0xFFuy; 0xFFuy |]
    let context, names, _ = observe context
    match Metering.Mbus.Protocol.Frames.ExtendedLinkLayer.EllCodec.decode source context with
    | Decoded ({ Value = Metering.Mbus.Protocol.Frames.ExtendedLinkLayer.Ell.NoDllEncryption header }, _) ->
        header.Payload.Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]
        header.Payload.Span.Offset |> should equal 3
        header.Ci.Span.Offset |> should equal 0
    | actual -> failwith $"Expected independent ELL decode, got {actual}"
    names.Contains "TPL" |> should be False

[<Fact>]
let ``orchestrator routes selection directly to its codec without TPL or APL`` () =
    // EN 13757-7, 8.4, Table 44: SND_UD, A=253, CI=52; wildcard selection.
    let userData = Array.append [| 0x53uy; 0xFDuy; 0x52uy |] (Array.create 8 0xFFuy)
    let length = byte userData.Length
    let checksum = userData |> Array.fold (+) 0uy
    let bytes = Array.concat [ [| 0x68uy; length; length; 0x68uy |]; userData; [| checksum; 0x16uy |] ]
    let source, context = contextFor bytes
    let context, names, _ = observe context
    let keys = CountingKeys(None)
    match MbusDecoder.decodeFrame keys source context with
    | Decoded ({ Value = WiredMbusFrame.VariableLength (_, LinkUserData.SelectionOfDevice _) }, _) -> ()
    | actual -> failwith $"Expected selection result, got {actual}"
    names.Contains "TPL" |> should be False
    names.Contains "APL" |> should be False
    keys.Calls |> should equal 0

[<Theory>]
[<InlineData(0x81)>]
[<InlineData(0x90)>]
let ``orchestrator rejects unsupported NWL and AFL without reaching TPL`` ci =
    let source, context = contextFor (wired [| byte ci |])
    let context, names, _ = observe context
    match MbusDecoder.decodeFrame (NoSecurityContext()) source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 6
        error.Msg |> should endWith "is not implemented."
    | actual -> failwith $"Expected unsupported layer, got {actual}"
    names.Contains "TPL" |> should be False

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(2)>]
let ``optional layer codecs preserve the original field when absent`` layer =
    // The known TPL CI proves absence of these layers; its body remains opaque.
    let root, context = contextFor [| 0xAAuy; 0xBBuy; 0x72uy; 0xFFuy; 0xFFuy |]
    let source = {
        root with
            Span = { root.Span with Offset = 2; Length = 3 }
            Value = root.Value.Slice(2)
    }
    let context, names, sources = observe context
    let remaining =
        match layer with
        | 1 ->
            match NwlCodec.tryDecode source context with
            | Decoded ((None, remaining), _) -> remaining
            | actual -> failwith $"Expected absent NWL, got {actual}"
        | 2 ->
            match AflCodec.tryDecode source context with
            | Decoded ((None, remaining), _) -> remaining
            | actual -> failwith $"Expected absent AFL, got {actual}"
        | _ ->
            match EllCodec.tryDecode source context with
            | Decoded ((None, remaining), _) -> remaining
            | actual -> failwith $"Expected absent ELL, got {actual}"
    remaining |> should equal source
    names.Count |> should equal 0
    sources.Count |> should equal 0

[<Fact>]
let ``optional ELL decode returns validated fields and the following byte field`` () =
    // EN 13757-4, 13.2.2, Table 44: CI 8C, CC and ACC, followed by other layers.
    let source, context = contextFor [| 0x8Cuy; 0x00uy; 0x08uy; 0xFFuy; 0xFFuy |]
    let context, names, _ = observe context
    match EllCodec.tryDecode source context with
    | Decoded ((Some ({ Value = Ell.NoDllEncryption header } as ell), remaining), _) ->
        ell.Span |> should equal source.Span
        remaining |> should equal header.Payload
        remaining.Span.Source |> should equal source.Span.Source
        remaining.Span.Offset |> should equal 3
        remaining.Span.Length |> should equal 2
        remaining.Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]
        AccessNumber.value header.Acc.Value |> should equal 8uy
    | actual -> failwith $"Expected present validated ELL and its payload, got {actual}"
    names.Contains "TPL" |> should be False

[<Theory>]
[<InlineData(0x8C)>]
[<InlineData(0x8D)>]
[<InlineData(0xFF)>]
let ``optional ELL decode propagates malformed unsupported and unknown CI errors`` ci =
    let source, context = contextFor [| byte ci |]
    match EllCodec.tryDecode source context with
    | DecodeFailed (ParseFailed error, _) -> error.Source |> should equal source.Span.Source
    | actual -> failwith $"Expected ELL or CI failure, got {actual}"

[<Theory>]
[<InlineData(0x81)>]
[<InlineData(0xFF)>]
let ``optional NWL decode propagates unsupported and unknown CI errors`` ci =
    let source, context = contextFor [| byte ci |]
    match NwlCodec.tryDecode source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Source |> should equal source.Span.Source
        error.Pos |> should equal 0
        if ci = 0x81 then error.Msg |> should endWith "is not implemented."
    | actual -> failwith $"Expected NWL or CI failure, got {actual}"

[<Fact>]
let ``orchestrator invokes optional NWL parsing after a present ELL`` () =
    let source, context = contextFor (wired [| 0x8Cuy; 0x00uy; 0x08uy; 0x81uy |])
    let context, names, _ = observe context
    match MbusDecoder.decodeFrame (NoSecurityContext()) source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 9
        error.Msg |> should endWith "is not implemented."
    | actual -> failwith $"Expected unsupported NWL after ELL, got {actual}"
    names.Contains "Extended Link Layer" |> should be True
    names.Contains "TPL" |> should be False

[<Theory>]
[<InlineData(0x90)>]
[<InlineData(0xFF)>]
let ``optional AFL decode propagates unsupported and unknown CI errors`` ci =
    let source, context = contextFor [| byte ci |]
    let context, names, sources = observe context
    match AflCodec.tryDecode source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Source |> should equal source.Span.Source
        error.Pos |> should equal 0
        if ci = 0x90 then error.Msg |> should endWith "is not implemented."
    | actual -> failwith $"Expected AFL or CI failure, got {actual}"
    names.Count |> should equal 0
    sources.Count |> should equal 0

[<Fact>]
let ``orchestrator invokes optional AFL parsing after a present ELL and absent NWL`` () =
    let source, context = contextFor (wired [| 0x8Cuy; 0x00uy; 0x08uy; 0x90uy |])
    let context, names, sources = observe context
    let keys = CountingKeys(None)
    match MbusDecoder.decode keys source context with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 9
        error.Msg |> should equal "Layer introduced by CI 0x90 is not implemented."
    | actual -> failwith $"Expected unsupported AFL after ELL, got {actual}"
    names.Contains "Extended Link Layer" |> should be True
    names.Contains "TPL" |> should be False
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0
    keys.Calls |> should equal 0

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``higher layers decoder returns headers and plain content without APL processing`` presentEll =
    // EN 13757-4, 13.2.2, Table 44: optional ELL CI 8C, CC, ACC.
    let bytes = if presentEll then [| 0x8Cuy; 0x00uy; 0x08uy; 0x50uy |] else [| 0x50uy |]
    let source, context = contextFor bytes
    let context, names, sources = observe context
    match HigherLayersDecoder.decode source context with
    | Decoded (higher, _) ->
        higher.Id |> should equal source.Id
        higher.Span |> should equal source.Span
        higher.Value.Ell.IsSome |> should equal presentEll
        higher.Value.Nwl.IsNone |> should be True
        higher.Value.Afl.IsNone |> should be True
        higher.Value.Tpl.Span.Offset |> should equal (if presentEll then 3 else 0)
        match higher.Value.Protection with
        | MbusProtectionInput.Plain bytes ->
            bytes.Span.Source |> should equal source.Span.Source
            bytes.Span.Offset |> should equal (if presentEll then 4 else 1)
            bytes.Value.Length |> should equal 0
        | actual -> failwith $"Expected unprocessed clear content, got {actual}"
    | actual -> failwith $"Expected independently decoded higher layers, got {actual}"
    names.Contains "TPL" |> should be True
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

[<Fact>]
let ``higher layers decoder preserves opaque plain APL bytes`` () =
    let source, context = contextFor [| 0x51uy; 0xFFuy; 0xFFuy |]
    match HigherLayersDecoder.decode source context with
    | Decoded ({ Value = { Protection = MbusProtectionInput.Plain bytes } }, _) ->
        bytes.Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]
        bytes.Span.Offset |> should equal 1
    | actual -> failwith $"Expected opaque APL bytes, got {actual}"

[<Theory>]
[<InlineData(0x0510, 16, 1)>]
[<InlineData(0x05F0, 32, 0)>]
[<InlineData(0x0500, 0, 1)>]
let ``higher layers decoder returns Mode 5 ranges and frame context without decrypting`` configuration encryptedLength clearLength =
    // EN 13757-7, 7.6.5 and 9.4.4: prefix, all N bits set, or zero encrypted blocks.
    let cipher = Array.create encryptedLength 0xFFuy
    let clear = Array.create clearLength 0x2Fuy
    let source, context = contextFor (Array.concat [longHeader (uint16 configuration); cipher; clear])
    let context, names, sources = observe context
    match HigherLayersDecoder.decode source context with
    | Decoded ({ Value = { Protection = MbusProtectionInput.Mode5 input } } as higher, _) ->
        higher.Id |> should equal source.Id
        higher.Span |> should equal source.Span
        input.Origin |> should equal (Tpl.aplData higher.Value.Tpl.Value)
        input.Encrypted.Span.Source |> should equal source.Span.Source
        input.Encrypted.Span.Offset |> should equal 13
        input.Encrypted.Span.Length |> should equal encryptedLength
        input.Encrypted.Value.ToArray() |> should equal cipher
        input.ClearSuffix.Span.Offset |> should equal (13 + encryptedLength)
        input.ClearSuffix.Value.ToArray() |> should equal clear
        input.Encrypted.Id = input.Origin.Id |> should be False
        input.ClearSuffix.Id = input.Encrypted.Id |> should be False
        IdNumber.value input.Context.Device.IdNum.Value |> should equal 12345678u
        AccessNumber.value input.Context.AccessNumber.Value |> should equal 8uy
    | actual -> failwith $"Expected prepared Mode 5 input, got {actual}"
    names.Contains "APL" |> should be False
    sources.Count |> should equal 0

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``grouped higher-layer parsing preserves raw fields and validation preserves their provenance`` presentEll =
    let body = [| 0x51uy; 0xFFuy; 0xFFuy |]
    let bytes = if presentEll then Array.append [| 0x8Cuy; 0x00uy; 0x08uy |] body else body
    let source, context = contextFor bytes
    let context, names, sources = observe context
    let parsed =
        match Core.parseValue HigherLayersDecoder.parse source context with
        | Decoded (raw, _) -> raw
        | actual -> failwith $"Expected structural header block, got {actual}"
    let raw = Field.withValue parsed source
    let fieldCount = names.Count
    let headers = HigherLayersDecoder.fromRaw raw |> validationValue
    headers.Id |> should equal raw.Id
    headers.Span |> should equal raw.Span
    headers.Value.Tpl.Id |> should equal raw.Value.Tpl.Id
    headers.Value.Tpl.Span |> should equal raw.Value.Tpl.Span
    Tpl.aplData headers.Value.Tpl.Value |> should equal (TplRaw.aplData raw.Value.Tpl.Value)
    headers.Value.Ell.IsSome |> should equal presentEll
    match raw.Value.Ell, headers.Value.Ell with
    | Some before, Some after ->
        after.Id |> should equal before.Id
        after.Span |> should equal before.Span
    | None, None -> ()
    | actual -> failwith $"Unexpected ELL presence: {actual}"
    names.Count |> should equal fieldCount
    sources.Count |> should equal 0
    names.Contains "APL" |> should be False

[<Fact>]
let ``grouped header parsing precedes pure validation and collects independent TPL failures`` () =
    // EN 13757-7, 7.5.1: A is not a BCD digit; 7.7.1, Tables 21-22: CC=01 is reserved.
    let header = longHeader 0x0514us
    header[4] <- 0xAAuy
    let bytes = Array.concat [[| 0x8Cuy; 0x00uy; 0x08uy |]; header; Array.create 16 0xFFuy]
    let source, context = contextFor bytes
    let context, names, sources = observe context
    let parsed =
        match Core.parseValue HigherLayersDecoder.parse source context with
        | Decoded (raw, _) -> raw
        | actual -> failwith $"Expected readable but invalid header block, got {actual}"
    let raw = Field.withValue parsed source
    let fieldCount = names.Count
    match raw.Value.Tpl.Value, HigherLayersDecoder.fromRaw raw with
    | TplRaw.LongHeader { Header = { Value = LongHeaderRaw.Mode5Raw header } }, Failed (failures, _) ->
        let issues = Failures.toList failures
        issues.Length |> should equal 2
        issues |> List.map _.FieldId |> Set.ofList
        |> should equal (Set.ofList [header.IdNum.Id; header.Cnf.Id])
        (TplRaw.aplData raw.Value.Tpl.Value).Value.Length |> should equal 16
    | actual -> failwith $"Expected both independent validation failures, got {actual}"
    names.Count |> should equal fieldCount
    names.Contains "Mode 5 encrypted data" |> should be False
    sources.Count |> should equal 0

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``optional NWL and AFL parsers leave the reader at the following CI when absent`` afl =
    let source, context = contextFor [| 0x51uy; 0xFFuy; 0xFFuy |]
    let parse = Metering.Common.Decoding.Parsers.Core.parser {
        if afl then
            let! layer = AflCodec.tryParse
            Assert.True(layer.IsNone)
        else
            let! layer = NwlCodec.tryParse
            Assert.True(layer.IsNone)
        return! TplCodec.parse
    }
    match Core.parse parse source context with
    | Decoded (tpl, _) ->
        tpl.Span |> should equal source.Span
        (TplRaw.aplData tpl.Value).Value.ToArray() |> should equal [| 0xFFuy; 0xFFuy |]
    | actual -> failwith $"Expected following TPL input untouched, got {actual}"
