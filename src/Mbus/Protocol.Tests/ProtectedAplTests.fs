module Metering.Mbus.Protocol.Tests.ProtectedAplTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Security
open Metering.Mbus.Protocol.Tests.TestSupport

let private payloadField id offset length =
    {
        Id = FieldId.create id
        Span = {
            Source = SourceId.root
            Offset = offset
            Length = length
        }
        Value = ReadOnlyMemory<byte>(Array.zeroCreate length)
    }

let private raw encrypted clearSuffix : ProtectedAplRaw =
    {
        OriginalPayload = payloadField 1 10 20
        EncryptedPart = encrypted
        ClearSuffix = clearSuffix
        Failure = SecurityContextNotUsable
    }

let private messages (value: ProtectedAplRaw) =
    match ProtectedApl.fromRaw value with
    | Failed (failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)

    | Passed _ ->
        failwith "Expected protected-APL validation failure"

let private shouldContain (expected: string) (actual: string list) =
    actual
    |> List.exists (fun message -> message.Contains(expected))
    |> should be True

[<Fact>]
let ``encrypted part starting after original start is rejected`` () =
    raw (payloadField 2 11 19) None
    |> messages
    |> shouldContain "must start at the original payload start"

[<Fact>]
let ``gap between encrypted part and clear suffix is rejected`` () =
    raw
        (payloadField 2 10 8)
        (Some (payloadField 3 19 11))
    |> messages
    |> shouldContain "must start exactly after the encrypted range"

[<Fact>]
let ``clear suffix not reaching original end is rejected`` () =
    raw
        (payloadField 2 10 8)
        (Some (payloadField 3 18 10))
    |> messages
    |> shouldContain "must end at the original payload end"

[<Fact>]
let ``encrypted part without suffix must cover complete original payload`` () =
    raw (payloadField 2 10 8) None
    |> messages
    |> shouldContain "without a clear suffix must end at the original payload end"

[<Fact>]
let ``full encryption is a valid complete partition`` () =
    raw (payloadField 2 10 20) None
    |> ProtectedApl.fromRaw
    |> validationValue
    |> fun protectedApl ->
        protectedApl.EncryptedPart.Span.Length
        |> should equal 20

[<Fact>]
let ``partial encryption is a valid complete partition`` () =
    raw
        (payloadField 2 10 8)
        (Some (payloadField 3 18 12))
    |> ProtectedApl.fromRaw
    |> validationValue
    |> fun protectedApl ->
        protectedApl.ClearSuffix.IsSome
        |> should be True

[<Fact>]
let ``zero-length encrypted prefix and full clear suffix is valid`` () =
    raw
        (payloadField 2 10 0)
        (Some (payloadField 3 10 20))
    |> ProtectedApl.fromRaw
    |> validationValue
    |> fun protectedApl ->
        protectedApl.EncryptedPart.Span.Length
        |> should equal 0
