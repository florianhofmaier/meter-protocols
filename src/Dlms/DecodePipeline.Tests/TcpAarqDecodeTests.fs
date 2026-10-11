module Metering.Dlms.DecodePipeline.Tests.TcpAarqDecodeTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.DecodePipeline.Tests.TestSupport
open Metering.Dlms.Protocol.CosemApdus.Decoding

let private systemTitle =
    [| 0x4Duy; 0x4Duy; 0x4Duy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy |]

let private encryptionKey =
    [| 0x00uy .. 0x0Fuy |]

let private authenticationKey =
    [| 0xD0uy .. 0xDFuy |]

let private validProtectedRequest () =
    protectedInitiateRequest systemTitle encryptionKey authenticationKey 1u

let private messages validation =
    match validation with
    | Passed _ -> failwith "Expected failed validation"
    | Failed (failures, _) ->
        failures
        |> Failures.toList
        |> List.map _.Message

[<Fact>]
let ``wrapper validation failure does not hide the AARQ`` () =
    let report =
        validProtectedRequest ()
        |> aarq true systemTitle
        |> wrapper 0x0002us
        |> decode (NoKeys())

    match report.Wrapper, report.Aarq, report.ProtectedApdu with
    | Stage.Reached (_, wrapperValidation),
      Stage.Reached _,
      Stage.Reached _ ->
        messages wrapperValidation
        |> List.exists _.Contains("unsupported wrapper version")
        |> should be True

    | actual ->
        failwith $"Expected invalid wrapper and reachable AARQ, got {actual}"

[<Fact>]
let ``AARQ validation failure does not hide readable user-information`` () =
    let report =
        validProtectedRequest ()
        |> aarq false systemTitle
        |> wrapper 0x0001us
        |> decode (NoKeys())

    match report.Aarq, report.ProtectedApdu with
    | Stage.Reached (_, aarqValidation), Stage.Reached _ ->
        messages aarqValidation
        |> List.exists _.Contains("missing application-context-name")
        |> should be True

    | actual ->
        failwith $"Expected invalid AARQ and reachable protected APDU, got {actual}"

[<Fact>]
let ``missing key retains the protected APDU as not attempted`` () =
    let report =
        validProtectedRequest ()
        |> aarq true systemTitle
        |> wrapper 0x0001us
        |> decode (NoKeys())

    match report.Exposure, report.InitiateRequest with
    | Transformation.NotAttempted
        (ExposureNotAttempted.PayloadUnavailable retained),
      Stage.NotReached InitiateRequestNotReached.PayloadProtected ->

        retained.Envelope.Span.Length > 0
        |> should be True

        retained.Problem
        |> should equal UnprotectionProblem.GlobalKeysUnavailable

    | actual ->
        failwith $"Expected retained protected APDU without key, got {actual}"

[<Fact>]
let ``authentication failure is a failed attempted transformation`` () =
    let report =
        invalidProtectedInitiateRequest 1u
        |> aarq true systemTitle
        |> wrapper 0x0001us
        |> decode (FixedKeys(encryptionKey, authenticationKey))

    match report.Exposure, report.InitiateRequest with
    | Transformation.Failed retained,
      Stage.NotReached InitiateRequestNotReached.PayloadProtected ->
        match retained.Problem with
        | UnprotectionProblem.CryptographicFailure issue ->
            issue.Message.Contains("authentication failed")
            |> should be True
        | actual ->
            failwith $"Expected cryptographic failure, got {actual}"

    | actual ->
        failwith $"Expected failed unprotection, got {actual}"

[<Fact>]
let ``successful unprotection creates a derived source and validates initiate-request`` () =
    let report =
        validProtectedRequest ()
        |> aarq true systemTitle
        |> wrapper 0x0001us
        |> decode (FixedKeys(encryptionKey, authenticationKey))

    match report.Exposure, report.InitiateRequest with
    | Transformation.Succeeded plain,
      Stage.Reached (_, Passed _) ->
        plain.Span.Source.Value |> should not' (equal 0)
        plain.Span.Offset |> should equal 0
        plain.Value.ToArray() |> should equal initiateRequest

    | actual ->
        failwith $"Expected decrypted and validated initiate-request, got {actual}"

[<Fact>]
let ``validation issues are collected across wrapper and AARQ stages`` () =
    let messages =
        validProtectedRequest ()
        |> aarq false systemTitle
        |> wrapper 0x0002us
        |> decode (NoKeys())
        |> TcpAarqDecode.validationIssues
        |> List.map _.Message

    messages
    |> List.exists _.Contains("unsupported wrapper version")
    |> should be True

    messages
    |> List.exists _.Contains("missing application-context-name")
    |> should be True
