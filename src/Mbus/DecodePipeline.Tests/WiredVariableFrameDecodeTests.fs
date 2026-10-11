module Metering.Mbus.DecodePipeline.Tests.WiredVariableFrameDecodeTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.DecodePipeline.Tests.TestSupport
open Metering.Mbus.Protocol.Frames.Decoding
open Metering.Mbus.Protocol.Frames.TransportLayer

let private failureMessages =
    function
    | Failed (failures, _) ->
        failures
        |> Failures.toList
        |> List.map _.Message
    | Passed _ ->
        failwith "Expected failed validation"

[<Fact>]
let ``mode zero reaches and validates every stage`` () =
    let report =
        frame 0x53uy 0x01uy [| 0x51uy |]
        |> decode (NoSecurityContext())

    match report.Dll, report.HigherLayers, report.Exposure, report.Apl with
    | Stage.Reached (_, Passed _),
      Stage.Reached (_, Passed _),
      Transformation.Succeeded source,
      Stage.Reached (_, Passed _) ->
        source.Span.Source.Value |> should equal 0

    | actual ->
        failwith $"Expected four successful stages, got {actual}"

[<Fact>]
let ``DLL validation failures do not block reachable higher layers`` () =
    let bytes = frame 0x41uy 0xFCuy [| 0x51uy |]
    bytes[bytes.Length - 2] <- bytes[bytes.Length - 2] + 1uy
    bytes[bytes.Length - 1] <- 0x00uy

    let report = decode (NoSecurityContext()) bytes

    match report.Dll with
    | Stage.Reached (_, validation) ->
        let messages = failureMessages validation

        [ "Invalid function code"; "Invalid AField"; "Checksum mismatch"; "End field is expected" ]
        |> List.iter (fun expected ->
            messages
            |> List.exists _.Contains(expected)
            |> should be True)

    | actual ->
        failwith $"Expected reached DLL, got {actual}"

    match report.HigherLayers, report.Apl with
    | Stage.Reached _, Stage.Reached _ -> ()
    | actual -> failwith $"Expected higher layers and APL to remain reachable, got {actual}"

    let collected =
        report
        |> WiredVariableFrameDecode.validationIssues
        |> List.map _.Message

    collected.Length |> should equal 4

[<Fact>]
let ``validation issues are collected across DLL and higher-layer stages`` () =
    let invalidLongHeader =
        [|
            0x72uy
            0xFAuy; 0x03uy; 0x04uy; 0x05uy
            0xFFuy; 0x01uy
            0xFFuy
            0xFFuy
            0x08uy
            0x00uy
            0x04uy; 0x00uy
        |]

    let bytes = frame 0x41uy 0xFCuy invalidLongHeader
    bytes[bytes.Length - 2] <- bytes[bytes.Length - 2] + 1uy
    bytes[bytes.Length - 1] <- 0x00uy

    let messages =
        decode (NoSecurityContext()) bytes
        |> WiredVariableFrameDecode.validationIssues
        |> List.map _.Message

    [
        "Invalid function code"
        "Invalid AField"
        "Checksum mismatch"
        "End field is expected"
        "Invalid BCD nibble"
        "Wildcard byte 0xFF is not allowed in manufacturer"
        "Wildcard byte 0xFF is not allowed in version"
        "Wildcard byte 0xFF is not allowed in device type"
        "Invalid ContentOfMessage"
    ]
    |> List.iter (fun expected ->
        messages
        |> List.exists _.Contains(expected)
        |> should be True)

[<Fact>]
let ``DLL raw model retains both FT 1.2 length fields`` () =
    let report =
        frame 0x53uy 0x01uy [| 0x51uy |]
        |> decode (NoSecurityContext())

    match report.Dll with
    | Stage.Reached (raw, Passed _) ->
        raw.Value.Length1.Value |> should equal 3uy
        raw.Value.Length1.Span.Offset |> should equal 1
        raw.Value.Length1.Span.Length |> should equal 1

        raw.Value.Length2.Value |> should equal 3uy
        raw.Value.Length2.Span.Offset |> should equal 2
        raw.Value.Length2.Span.Length |> should equal 1

    | actual -> failwith $"Expected a valid reached DLL stage, got {actual}"

[<Fact>]
let ``unequal FT 1.2 length fields are a DLL validation failure`` () =
    let bytes = frame 0x53uy 0x01uy [| 0x51uy |]
    bytes[2] <- bytes[1] + 1uy

    let report = decode (NoSecurityContext()) bytes

    match report.Dll, report.HigherLayers with
    | Stage.Reached (_, validation), Stage.Reached _ ->
        failureMessages validation
        |> List.exists _.Contains("Length 2 must equal Length 1")
        |> should be True

    | actual ->
        failwith $"Expected an invalid reached DLL and reachable higher layers, got {actual}"

[<Fact>]
let ``malformed DLL marks dependent stages as not reached`` () =
    let bytes = frame 0x53uy 0x01uy [| 0x51uy |]
    bytes[0] <- 0x69uy

    let report = decode (NoSecurityContext()) bytes

    match report.Dll, report.HigherLayers, report.Exposure, report.Apl with
    | Stage.Malformed _,
      Stage.NotReached HigherLayersNotReached.DllMalformed,
      Transformation.NotAttempted ExposureNotAttempted.DllMalformed,
      Stage.NotReached AplNotReached.DllMalformed -> ()
    | actual -> failwith $"Unexpected reachability result: {actual}"

[<Fact>]
let ``malformed higher layers retain DLL and block exposure and APL`` () =
    let report =
        frame 0x08uy 0x01uy [| 0x72uy |]
        |> decode (NoSecurityContext())

    match report.Dll, report.HigherLayers, report.Exposure, report.Apl with
    | Stage.Reached _,
      Stage.Malformed _,
      Transformation.NotAttempted ExposureNotAttempted.HigherLayersMalformed,
      Stage.NotReached AplNotReached.HigherLayersMalformed -> ()
    | actual -> failwith $"Unexpected reachability result: {actual}"

[<Fact>]
let ``mode five without key retains encrypted prefix and clear suffix`` () =
    let encrypted = Array.zeroCreate<byte> 16
    let suffix = [| 0x0Fuy |]

    let report =
        Array.concat [ mode5LongHeader 0x0510us; encrypted; suffix ]
        |> frame 0x08uy 0x01uy
        |> decode (NoSecurityContext())

    match report.Exposure, report.Apl with
    | Transformation.NotAttempted (ExposureNotAttempted.PayloadUnavailable protectedPayload),
      Stage.NotReached AplNotReached.PayloadProtected ->
        protectedPayload.EncryptedPrefix.Value.Length |> should equal 16
        protectedPayload.ClearSuffix.Value.Value.ToArray() |> should equal suffix

        match protectedPayload.Problem with
        | PayloadExposureProblem.Mode5ContextUnavailable _ -> ()
        | actual -> failwith $"Expected unavailable Mode 5 context, got {actual}"

    | actual -> failwith $"Expected retained protected payload, got {actual}"

[<Fact>]
let ``mode five verification failure is a failed attempted transformation`` () =
    let report =
        Array.append (mode5LongHeader 0x05F0us) (Array.zeroCreate 16)
        |> frame 0x08uy 0x01uy
        |> decode (FixedMode5Context (Array.create 16 0xAAuy))

    match report.Exposure, report.Apl with
    | Transformation.Failed protectedPayload,
      Stage.NotReached AplNotReached.PayloadProtected ->
        protectedPayload.EncryptedPrefix.Value.Length |> should equal 16

        match protectedPayload.Problem with
        | PayloadExposureProblem.VerificationFailed -> ()
        | actual -> failwith $"Expected verification failure, got {actual}"
    | actual -> failwith $"Expected a failed attempted transformation, got {actual}"

[<Fact>]
let ``mode five all-remaining layout rejects a non-block-sized payload before key lookup`` () =
    let report =
        Array.append (mode5LongHeader 0x05F0us) (Array.zeroCreate 15)
        |> frame 0x08uy 0x01uy
        |> decode (NoSecurityContext())

    match report.Exposure with
    | Transformation.Failed {
        Problem = PayloadExposureProblem.InvalidMode5Layout failures
      } ->
        let message =
            failures
            |> Failures.toList
            |> List.exactlyOne
            |> _.Message

        message.Contains("multiple of 16") |> should be True
        message.Contains("Table 48") |> should be True
    | actual -> failwith $"Expected invalid Mode 5 layout, got {actual}"

[<Fact>]
let ``raw higher-layer fields retain their exact source spans`` () =
    let report =
        Array.append (mode5LongHeader 0x0510us) (Array.zeroCreate 16)
        |> frame 0x08uy 0x01uy
        |> decode (NoSecurityContext())

    match report.HigherLayers with
    | Stage.Reached (higher, _) ->
        match higher.Value.Tpl.Value with
        | TplRaw.LongHeader { Header = { Value = LongHeaderRaw.Mode5Raw header } } ->
            header.Cnf.Span.Offset |> should equal 17
            header.Cnf.Span.Length |> should equal 2
            ConfigurationFieldBitsRaw.value header.Cnf.Value |> should equal 0x0510us
        | actual -> failwith $"Expected Mode 5 long header, got {actual}"
    | actual -> failwith $"Expected reached higher-layer stage, got {actual}"

[<Fact>]
let ``mode five success creates a derived source before parsing APL`` () =
    let key = [| 0x00uy .. 0x0Fuy |]

    let cipherText =
        [|
            0x23uy; 0xE4uy; 0x18uy; 0x2Cuy
            0x34uy; 0x40uy; 0xD7uy; 0x2Auy
            0x04uy; 0x8Auy; 0x7Cuy; 0x6Cuy
            0x36uy; 0xE9uy; 0xCAuy; 0xBEuy
        |]

    let report =
        Array.append (mode5LongHeader 0x05F0us) cipherText
        |> frame 0x08uy 0x01uy
        |> decode (FixedMode5Context key)

    match report.Exposure, report.Apl with
    | Transformation.Succeeded source, Stage.Reached _ ->
        source.Span.Source.Value |> should not' (equal 0)
        source.Span.Offset |> should equal 0
    | actual -> failwith $"Expected decrypted and parsed APL, got {actual}"

[<Fact>]
let ``partial mode five combines decrypted prefix and clear suffix in one derived source`` () =
    let key = [| 0x00uy .. 0x0Fuy |]

    let encryptedPrefix =
        [|
            0x23uy; 0xE4uy; 0x18uy; 0x2Cuy
            0x34uy; 0x40uy; 0xD7uy; 0x2Auy
            0x04uy; 0x8Auy; 0x7Cuy; 0x6Cuy
            0x36uy; 0xE9uy; 0xCAuy; 0xBEuy
        |]

    let report =
        Array.concat [ mode5LongHeader 0x0510us; encryptedPrefix; [| 0x2Fuy |] ]
        |> frame 0x08uy 0x01uy
        |> decode (FixedMode5Context key)

    match report.Exposure, report.Apl with
    | Transformation.Succeeded source, Stage.Reached _ ->
        source.Span.Source.Value |> should not' (equal 0)
        source.Value.Length |> should equal 15
        source.Value.Span[14] |> should equal 0x2Fuy
    | actual -> failwith $"Expected combined partial-encryption source, got {actual}"
