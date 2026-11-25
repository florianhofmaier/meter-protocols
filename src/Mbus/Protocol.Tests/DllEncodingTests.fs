module Metering.Mbus.Protocol.Tests.DllEncodingTests

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Tests.TestSupport

let private decode bytes =
    parseExactly DllCodec.parse bytes |> DllCodec.validate |> validationValue

let private encode frame =
    match DllCodec.encode frame with
    | Ok bytes -> bytes
    | Error error -> failwith $"Unexpected DLL encoding failure: {error}"

// These fields are taken from real input; creating an output does not require new provenance.
let private fixedFrame =
    match (decode [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]).Value with
    | Dll.FixedLength frame -> frame
    | _ -> failwith "Expected fixed-length fixture"

let private linkUserData bytes =
    bytesField bytes |> LinkUserDataBytes.validate |> validationValue

let private variableFrame = {
    CField =
        fixedFrame.CField
        |> Field.withValue (
            CField.Secondary {
                Acd = false
                Dfc = false
                Func = fixedFrame.CField |> Field.withValue SecondaryFunction.ResponseUserData
            }
        )
    AField = fixedFrame.AField
    LinkUserData = linkUserData [| 0xFFuy; 0xFFuy |]
}

[<Fact>]
let ``DLL encoding reproduces ACK and the standard reset and readout examples`` () =
    // IEC 60870-5-2, 3.2, p. 14; EN 13757-2, F.1/F.2, p. 37.
    // 40h + FFh wraps to the standard's 3Fh checksum.
    let examples = [
        [| 0xE5uy |]
        [| 0x10uy; 0x40uy; 0xFFuy; 0x3Fuy; 0x16uy |]
        [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]
    ]

    for expected in examples do
        (decode expected).Value |> encode |> should equal expected

[<Fact>]
let ``variable frame encoding includes C A and opaque Link User Data in length and checksum`` () =
    let expected = [| 0x68uy; 4uy; 4uy; 0x68uy; 8uy; 1uy; 0xFFuy; 0xFFuy; 7uy; 0x16uy |]
    encode (Dll.VariableLength variableFrame) |> should equal expected
    (decode expected).Value |> encode |> should equal expected

[<Fact>]
let ``primary functions and FCB FCV bits are encoded independently`` () =
    // Bit representation only; this does not assert valid link-procedure combinations.
    for fn, code in
        [
            PrimaryFunction.LinkReset, 0uy
            PrimaryFunction.SendUserData, 3uy
            PrimaryFunction.SendUserDataNoResponse, 4uy
            PrimaryFunction.RequestUserDataClass1, 10uy
            PrimaryFunction.RequestUserDataClass2, 11uy
        ] do
        for fcb in [ false; true ] do
            for fcv in [ false; true ] do
                let control =
                    CField.Primary {
                        Fcb = fcb
                        Fcv = fcv
                        Func = fixedFrame.CField |> Field.withValue fn
                    }

                let output =
                    Dll.FixedLength {
                        fixedFrame with
                            CField = fixedFrame.CField |> Field.withValue control
                    }
                    |> encode

                let expected =
                    0x40uy
                    ||| code
                    ||| (if fcb then 0x20uy else 0uy)
                    ||| (if fcv then 0x10uy else 0uy)

                output[1] |> should equal expected

                match (decode output).Value with
                | Dll.FixedLength frame -> frame.CField.Value |> should equal control
                | _ -> failwith "Expected fixed length"

[<Fact>]
let ``response ACD and DFC bits are encoded independently`` () =
    for acd in [ false; true ] do
        for dfc in [ false; true ] do
            let control =
                CField.Secondary {
                    Acd = acd
                    Dfc = dfc
                    Func = fixedFrame.CField |> Field.withValue SecondaryFunction.ResponseUserData
                }

            let output =
                Dll.VariableLength {
                    variableFrame with
                        CField = fixedFrame.CField |> Field.withValue control
                }
                |> encode

            output[4]
            |> should
                equal
                (8uy ||| (if acd then 0x20uy else 0uy) ||| (if dfc then 0x10uy else 0uy))

            match (decode output).Value with
            | Dll.VariableLength frame ->
                match frame.CField.Value with
                | CField.Secondary secondary ->
                    secondary.Acd |> should equal acd
                    secondary.Dfc |> should equal dfc
                    secondary.Func.Value |> should equal SecondaryFunction.ResponseUserData
                | _ -> failwith "Expected secondary control"
            | _ -> failwith "Expected variable length"

[<Fact>]
let ``all modeled address alternatives encode to their standard byte`` () =
    let configured value =
        PrimAdr.tryCreate value |> Option.get |> AField.Configured

    let addresses = [
        AField.Unconfigured, 0uy
        configured 1uy, 1uy
        configured 250uy, 250uy
        AField.RepeaterMgmt, 251uy
        AField.SelectionOfDevice, 253uy
        AField.Diagnosis, 254uy
        AField.Broadcast, 255uy
    ]

    for address, expected in addresses do
        let output =
            Dll.FixedLength {
                fixedFrame with
                    AField = fixedFrame.AField |> Field.withValue address
            }
            |> encode

        output[2] |> should equal expected

        match (decode output).Value with
        | Dll.FixedLength frame -> frame.AField.Value |> should equal address
        | _ -> failwith "Expected fixed length"

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(253)>]
let ``variable frame boundaries preserve the entire Link User Data`` count =
    let payload = Array.init count (fun index -> byte (index % 256))

    let frame = {
        variableFrame with
            LinkUserData = linkUserData payload
    }

    let output = Dll.VariableLength frame |> encode
    output.Length |> should equal (count + 8)
    output[1] |> should equal (byte (count + 2))
    output[2] |> should equal output[1]
    output[output.Length - 1] |> should equal 0x16uy

    match (decode output).Value with
    | Dll.VariableLength decoded ->
        (LinkUserDataBytes.bytes decoded.LinkUserData.Value).ToArray()
        |> should equal payload
    | _ -> failwith "Expected variable length"

[<Theory>]
[<InlineData(254)>]
[<InlineData(4096)>]
let ``oversized Link User Data is rejected before a valid frame can be constructed`` count =
    let input = Array.create count 0xAAuy

    match LinkUserDataBytes.create (ReadOnlyMemory input) with
    | Error message -> message.Contains "253" |> should be True
    | Ok _ -> failwith "Expected Link User Data construction error"

    input |> should equal (Array.create count 0xAAuy)

[<Fact>]
let ``encoding ignores provenance and produces independent output`` () =
    let payload = [| 0x68uy; 0x10uy; 0xE5uy |]

    let payloadField = {
        linkUserData payload with
            Id = FieldId.create 77
            Span = {
                Source = SourceId.root
                Offset = 999
                Length = 0
            }
    }

    let input =
        Dll.VariableLength {
            variableFrame with
                LinkUserData = payloadField
        }

    let first = encode input
    let second = encode input
    first |> should equal second
    first[6] <- 0uy
    payload[0] |> should equal 0x68uy
    second[6] |> should equal 0x68uy
    payload[1] <- 0uy
    second[7] |> should equal 0x10uy

[<Fact>]
let ``reserved received C bit is not reintroduced into encoded control`` () =
    let received = [| 0x10uy; 0xDBuy; 1uy; 0xDCuy; 0x16uy |]

    (decode received).Value
    |> encode
    |> should equal [| 0x10uy; 0x5Buy; 1uy; 0x5Cuy; 0x16uy |]

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(253)>]
let ``Link User Data constructor accepts every boundary without copying its memory`` count =
    let input = Array.create (count + 2) 0xAAuy
    let source = ReadOnlyMemory(input, 1, count)

    match LinkUserDataBytes.create source with
    | Ok bounded ->
        LinkUserDataBytes.bytes bounded |> should equal source

        if count > 0 then
            input[1] <- 0x55uy
            (LinkUserDataBytes.bytes bounded).Span[0] |> should equal 0x55uy

        (LinkUserDataBytes.bytes bounded).Length |> should equal count
    | Error error -> failwith $"Unexpected construction error: {error}"

[<Fact>]
let ``Link User Data validation retains provenance and locates a length failure on its raw field``
    ()
    =
    let valid = bytesField (Array.create 253 0uy)
    let bounded = LinkUserDataBytes.validate valid |> validationValue
    bounded.Id |> should equal valid.Id
    bounded.Span |> should equal valid.Span
    let invalid = bytesField (Array.create 254 0uy)

    match LinkUserDataBytes.validate invalid with
    | Failed(failures, _) ->
        let issues = Failures.toList failures
        issues.Length |> should equal 1
        issues.Head.FieldId |> should equal invalid.Id
    | _ -> failwith "Expected raw-field validation failure"

[<Fact>]
let ``frame validation enforces the bound on manually constructed raw Link User Data`` () =
    let input = [| 0x68uy; 4uy; 4uy; 0x68uy; 8uy; 1uy; 0xFFuy; 0xFFuy; 7uy; 0x16uy |]
    let raw = parseExactly DllCodec.parse input

    let oversized = {
        bytesField (Array.create 254 0uy) with
            Id = FieldId.create 77
    }

    let changed =
        match raw.Value with
        | DllRaw.VariableLength frame ->
            let userData = {
                frame.UserData.Value with
                    LinkUserData = oversized
            }

            DllRaw.VariableLength {
                frame with
                    UserData = frame.UserData |> Field.withValue userData
            }
        | _ -> failwith "Expected variable frame"

    match DllCodec.validate (raw |> Field.withValue changed) with
    | Failed(failures, _) ->
        let issues = Failures.toList failures
        issues.Length |> should equal 1
        issues.Head.FieldId |> should equal oversized.Id
    | _ -> failwith "Expected bounded Link User Data validation to fail"

let private frameWriters () = [
    SingleCharacterFrame.write, [| 0xE5uy |]
    FixedLengthFrame.write (CField.write fixedFrame.CField.Value) fixedFrame.AField.Value,
    [| 0x10uy; 0x5Buy; 1uy; 0x5Cuy; 0x16uy |]
    VariableLengthFrame.write
        (CField.write variableFrame.CField.Value)
        variableFrame.AField.Value
        (LinkUserDataBytes.write variableFrame.LinkUserData.Value),
    [| 0x68uy; 4uy; 4uy; 0x68uy; 8uy; 1uy; 0xFFuy; 0xFFuy; 7uy; 0x16uy |]
]

[<Fact>]
let ``frame writers compose at a nonzero offset and checksum only their own user data`` () =
    for write, expected in frameWriters () do
        let combined =
            writer {
                do! writeU8 0xA5uy
                do! writeU8 0x5Auy
                do! write
                do! writeU8 0xCCuy
            }

        match Writer.run (expected.Length + 3) combined with
        | Ok output ->
            output
            |> should equal (Array.concat [ [| 0xA5uy; 0x5Auy |]; expected; [| 0xCCuy |] ])
        | Error error -> failwith $"Unexpected frame writer error: {error}"

[<Fact>]
let ``frame writers propagate insufficient destination capacity`` () =
    for write, expected in frameWriters () do
        let capacity = expected.Length - 1

        match Writer.run capacity write with
        | Error error -> error.Pos |> should equal capacity
        | Ok _ -> failwith "Expected frame writer capacity failure"
