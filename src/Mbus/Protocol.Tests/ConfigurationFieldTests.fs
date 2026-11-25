module Metering.Mbus.Protocol.Tests.ConfigurationFieldTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Tests.TestSupport

let private expectedSpan: SourceSpan = {
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

let private assertValidatedField (bits: Field<ConfigurationFieldBitsRaw>) (validated: Field<_>) =

    validated.Id |> should equal bits.Id
    validated.Span |> should equal bits.Span
    validated.Span.Length |> should equal 2

let private assertMode5 bytes expectedEncryptedLength =
    let raw = parseExactly ConfigurationFieldRaw.parse bytes

    match raw.Value with
    | Mode5Raw bits ->
        assertRawField raw bits

        let validated = ConfigurationFieldMode5.validate bits |> validationValue

        assertValidatedField bits validated

        validated.Value.ContentOfMsg |> should equal ContentOfMessage.StandardData

        NumberOfEncryptedBlocks.value validated.Value.NumOfEncryptedBlocks
        |> should equal expectedEncryptedLength

    | actual -> failwith $"Expected Mode5Raw, got %A{actual}"

[<Fact>]
let ``mode five zero N bits validates as no encrypted data`` () = assertMode5 [| 0x00uy; 0x05uy |] 0

[<Fact>]
let ``mode five one N bit validates as one fixed encrypted block`` () =
    assertMode5 [| 0x10uy; 0x05uy |] 1

[<Fact>]
let ``mode five fourteen N bits validates as fourteen fixed encrypted blocks`` () =
    assertMode5 [| 0xE0uy; 0x05uy |] 14

[<Fact>]
let ``mode five all N bits validates as all remaining data encrypted`` () =
    assertMode5 [| 0xF0uy; 0x05uy |] 15

[<Fact>]
let ``mode zero preserves high low-byte nibble without mode five encrypted length`` () =
    let raw = parseExactly ConfigurationFieldRaw.parse [| 0xF0uy; 0x00uy |]

    match raw.Value with
    | Mode0Raw bits ->
        assertRawField raw bits

        let validated =
            validateField ConfigurationFieldMode0.validate bits |> validationValue

        assertValidatedField bits validated

        validated.Value.ContentOfMsg |> should equal ContentOfMessage.StandardData

    | actual -> failwith $"Expected Mode0Raw, got %A{actual}"

let private assertUnsupportedMode bytes (expectedText: string) =
    match parseResult ConfigurationFieldRaw.parse bytes with
    | Error error ->
        error.Pos |> should equal 0
        error.Msg.Contains(expectedText) |> should be True
    | Ok actual -> failwith $"Expected unsupported-mode parser failure, got %A{actual}"

[<Fact>]
let ``unsupported standard mode stops configuration parsing`` () =
    assertUnsupportedMode [| 0x00uy; 0x01uy |] "Unsupported security mode 1"

[<Fact>]
let ``reserved mode stops configuration parsing`` () =
    assertUnsupportedMode [| 0x00uy; 0x1Fuy |] "Unsupported security mode 31"

[<Fact>]
let ``mode zero is mapped from configuration bits`` () =
    Mode.tryMap 0x0000us |> should equal (Some Mode.Mode0)

[<Fact>]
let ``mode five is mapped from configuration bits`` () =
    Mode.tryMap 0x0500us |> should equal (Some Mode.Mode5)

[<Fact>]
let ``unimplemented mode one has no supported mapping`` () =
    Mode.tryMap 0x0100us |> should equal None

[<Fact>]
let ``unimplemented mode six has no supported mapping`` () =
    Mode.tryMap 0x0600us |> should equal None

[<Fact>]
let ``configuration accessibility is the joint B A function from table 23`` () =
    // EN 13757-7:2018, 7.7.1, Table 23 and Tables 27/31.
    let cases = [
        0x00uy, Accessibility.NoAccessWindow
        0x40uy, Accessibility.BidirectionalAccessWithoutWindow
        0x80uy, Accessibility.ShortAccessWindow
        0xC0uy, Accessibility.UnlimitedAccess
    ]

    for high, expected in cases do
        for mode in [ 0uy; 5uy ] do
            let raw = parseExactly ConfigurationFieldRaw.parse [| 0uy; high ||| mode |]

            match raw.Value with
            | Mode0Raw bits ->
                let configuration = ConfigurationFieldMode0.validate bits |> validationValue
                configuration.Accessibility |> should equal expected
            | Mode5Raw bits ->
                let configuration = ConfigurationFieldMode5.validate bits |> validationValue
                configuration.Value.Accessibility |> should equal expected

[<Fact>]
let ``configuration functions retain their named H R S meanings`` () =
    let raw = parseExactly ConfigurationFieldRaw.parse [| 3uy; 0x25uy |]
    let bits = ConfigurationFieldRaw.bits raw.Value

    let configuration =
        ConfigurationFieldMode5.validate bits |> validationValue |> _.Value

    configuration.HopCounter |> should equal HopCounter.One
    configuration.RepeaterAccess |> should equal RepeaterAccess.Set
    configuration.Synchronized |> should equal Synchronization.Synchronized
    configuration.Accessibility |> should equal Accessibility.NoAccessWindow
    let clear = ConfigurationFieldMode0.standardData
    clear.HopCounter |> should equal HopCounter.Zero
    clear.RepeaterAccess |> should equal RepeaterAccess.NotSet
    clear.Synchronized |> should equal Synchronization.NotSynchronized

[<Fact>]
let ``configuration validators reject raw bits belonging to another mode`` () =
    let bits mode =
        parseExactly ConfigurationFieldRaw.parse [| 0uy; mode |]
        |> _.Value
        |> ConfigurationFieldRaw.bits

    match ConfigurationFieldMode0.validate (bits 5uy) with
    | Metering.Common.Decoding.Validators.Core.Failed _ -> ()
    | _ -> failwith "Mode-5 bits must not construct a mode-0 configuration"

    match ConfigurationFieldMode5.validate (bits 0uy) with
    | Metering.Common.Decoding.Validators.Core.Failed _ -> ()
    | _ -> failwith "Mode-0 bits must not construct a mode-5 configuration"
