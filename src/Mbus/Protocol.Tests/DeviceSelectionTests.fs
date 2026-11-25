module Metering.Mbus.Protocol.Tests.DeviceSelectionTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Tests.TestSupport

// EN 13757-7:2018, 8.4, Table 44, pp. 50-51; 7.3, pp. 26-28.
let private address = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]

let private raw bytes =
    Array.append [| 0x52uy |] bytes |> parseExactly SelectionCodec.parse

let private device bytes =
    let fields =
        match (raw bytes).Value with
        | DeviceSelectionRaw.BySecondaryAddress fields -> fields
        | _ -> failwith "Expected secondary address"

    DeviceIdentification.validateElements fields.IdNum fields.Mfr fields.Version fields.DevType
    |> validationValue
    |> DeviceIdentification.toMeterAddress

let private selection bytes =
    raw bytes |> SelectionCodec.validate |> validationValue |> Field.value

let private matches pattern =
    DeviceSelection.matches (selection pattern) (device address)

[<Fact>]
let ``exact secondary address matches and each different field rejects`` () =
    matches address |> should be True

    for index in 0..7 do
        let other = Array.copy address
        other[index] <- other[index] ^^^ 1uy
        matches other |> should be False

[<Fact>]
let ``each identification digit independently supports F wildcard`` () =
    for index in 0..3 do
        for mask in [ 0x0Fuy; 0xF0uy ] do
            let pattern = Array.copy address
            pattern[index] <- pattern[index] ||| mask
            matches pattern |> should be True
            let different = Array.copy pattern
            let otherIndex = (index + 1) % 4
            different[otherIndex] <- different[otherIndex] ^^^ 1uy
            matches different |> should be False

[<Fact>]
let ``binary wildcards apply independently to both manufacturer bytes version and device type`` () =
    for index in 4..7 do
        let pattern = Array.copy address
        pattern[index] <- 0xFFuy
        matches pattern |> should be True
        let different = Array.copy pattern
        let otherIndex = if index = 4 then 5 else 4
        different[otherIndex] <- different[otherIndex] ^^^ 1uy
        matches different |> should be False

[<Fact>]
let ``F nibble in binary fields is a literal rather than a wildcard`` () =
    for index in 4..7 do
        for mask in [ 0x0Fuy; 0xF0uy ] do
            let pattern = Array.copy address
            pattern[index] <- pattern[index] ||| mask
            matches pattern |> should be False

[<Fact>]
let ``fully wildcarded selection matches identification number boundaries`` () =
    let pattern = selection (Array.create 8 0xFFuy)

    for id in [ 0x00uy; 0x99uy ] do
        let bytes = Array.copy address

        for index in 0..3 do
            bytes[index] <- id

        DeviceSelection.matches pattern (device bytes) |> should be True

[<Fact>]
let ``selection rejects A through E in every identification nibble`` () =
    for index in 0..3 do
        for shift in [ 0; 4 ] do
            for digit in 0xAuy .. 0xEuy do
                let pattern = Array.copy address
                let mask = 0xFuy <<< shift
                pattern[index] <- (pattern[index] &&& (~~~mask)) ||| (digit <<< shift)

                match raw pattern |> SelectionCodec.validate with
                | Failed _ -> ()
                | actual -> failwith $"Expected invalid BCD selection, got {actual}"

[<Fact>]
let ``selection rejects incomplete address wrong CI and trailing bytes`` () =
    let cases = [
        Array.append [| 0x52uy |] address[..6]
        Array.append [| 0x51uy |] address
        Array.concat [ [| 0x52uy |]; address; [| 0x00uy |] ]
    ]

    for bytes in cases do
        match parseResult SelectionCodec.parse bytes with
        | Error _ -> ()
        | Ok _ -> failwith "Expected malformed selection to fail parsing"

[<Fact>]
let ``extended selection is explicitly rejected until fabrication matching is supported`` () =
    // EN 13757-7:2018, 8.5, Table 45, p. 52: all additional criteria must match.
    let bytes =
        Array.append address [| 0x0Cuy; 0x78uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

    match raw bytes |> SelectionCodec.validate with
    | Failed(failures, _) ->
        Failures.toList failures
        |> List.exists (fun failure -> failure.Message.Contains "Extended device selection")
        |> should be True
    | actual -> failwith $"Expected unsupported extended selection, got {actual}"

[<Fact>]
let ``CI 52 alone selects all devices without fabricated address fields`` () =
    let parsed = parseExactly SelectionCodec.parse [| 0x52uy |]
    parsed.Span.Length |> should equal 1

    match parsed.Value with
    | DeviceSelectionRaw.AllDevices ci -> ci.Span.Length |> should equal 1
    | actual -> failwith $"Expected all-devices selection, got {actual}"

    let validated = SelectionCodec.validate parsed |> validationValue
    validated.Span |> should equal parsed.Span
    validated.Value |> should equal DeviceSelection.AllDevices
    DeviceSelection.matches validated.Value (device address) |> should be True
