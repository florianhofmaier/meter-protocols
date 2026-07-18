module Metering.Mbus.Protocol.Tests.ConfigurationFieldTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Types
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Tests.TestSupport

type private ExpectedEncryptedLength =
    | ExpectedNoEncryptedData
    | ExpectedFixedEncryptedBlocks of int
    | ExpectedAllRemainingDataEncrypted

let private expectedSpan : SourceSpan =
    {
        Source = SourceId.root
        Offset = 0
        Length = 2
    }

let private assertRawField
    (raw: Field<ConfigurationFieldRaw>)
    (bits: Field<ConfigurationFieldBitsRaw>)
    =

    raw.Id |> should equal bits.Id
    raw.Span |> should equal bits.Span
    bits.Id |> should equal (FieldId.create 0)
    bits.Span |> should equal expectedSpan

let private assertValidatedField
    (bits: Field<ConfigurationFieldBitsRaw>)
    (validated: Field<_>)
    =

    validated.Id |> should equal bits.Id
    validated.Span |> should equal bits.Span
    validated.Span.Length |> should equal 2

let private assertEncryptedLength expected actual =
    match expected, actual with
    | ExpectedNoEncryptedData, NoEncryptedData ->
        ()

    | ExpectedFixedEncryptedBlocks expectedCount, FixedEncryptedBlocks blocks ->
        EncryptedBlockCount.value blocks |> should equal expectedCount

    | ExpectedAllRemainingDataEncrypted, AllRemainingDataEncrypted ->
        ()

    | _ ->
        failwith $"Expected encrypted length %A{expected}, got %A{actual}"

let private assertMode5 bytes expectedEncryptedLength =
    let raw =
        parseExactly ConfigurationFieldRaw.parse bytes

    match raw.Value with
    | Mode5Raw bits ->
        assertRawField raw bits

        let validated =
            ConfigurationFieldMode5.fromRaw bits
            |> validationValue

        assertValidatedField bits validated
        validated.Value.Mode |> should equal Mode.Mode5
        validated.Value.ContentOfMsg |> should equal ContentOfMessage.StandardData
        assertEncryptedLength expectedEncryptedLength validated.Value.EncryptedLength

    | actual ->
        failwith $"Expected Mode5Raw, got %A{actual}"

[<Fact>]
let ``mode five zero N bits validates as no encrypted data`` () =
    assertMode5 [| 0x00uy; 0x05uy |] ExpectedNoEncryptedData

[<Fact>]
let ``mode five one N bit validates as one fixed encrypted block`` () =
    assertMode5 [| 0x10uy; 0x05uy |] (ExpectedFixedEncryptedBlocks 1)

[<Fact>]
let ``mode five fourteen N bits validates as fourteen fixed encrypted blocks`` () =
    assertMode5 [| 0xE0uy; 0x05uy |] (ExpectedFixedEncryptedBlocks 14)

[<Fact>]
let ``mode five all N bits validates as all remaining data encrypted`` () =
    assertMode5 [| 0xF0uy; 0x05uy |] ExpectedAllRemainingDataEncrypted

[<Fact>]
let ``mode zero preserves high low-byte nibble without mode five encrypted length`` () =
    let raw =
        parseExactly ConfigurationFieldRaw.parse [| 0xF0uy; 0x00uy |]

    match raw.Value with
    | Mode0Raw bits ->
        assertRawField raw bits

        let validated =
            ConfigurationFieldMode0.fromRaw bits
            |> validationValue

        assertValidatedField bits validated
        validated.Value.Mode |> should equal Mode.Mode0
        validated.Value.ContentOfMsg |> should equal ContentOfMessage.StandardData

    | actual ->
        failwith $"Expected Mode0Raw, got %A{actual}"

[<Fact>]
let ``unsupported mode is rejected by configuration field parser`` () =
    match runExactly (reader [| 0x00uy; 0x01uy |]) trace ConfigurationFieldRaw.parse with
    | Error error ->
        error.Source |> should equal SourceId.root
        error.Pos |> should equal 0
        error.Msg |> should equal "Encryption mode not supported: 256"

    | Ok value ->
        failwith $"Expected parser failure, got %A{value}"
