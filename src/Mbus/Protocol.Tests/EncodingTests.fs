module Metering.Mbus.Protocol.Tests.EncodingTests

open System
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Xunit
open FsUnit.Xunit
open Metering.Common.BaseWriters.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Tests.TestSupport
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.DataInfoBlocks
open Metering.Mbus.Protocol.Records.ValueInfoBlocks
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Messages

let private written writer =
    match Writer.run 261 writer with
    | Ok bytes -> bytes
    | Error error -> failwith error.Msg

let private expectError result =
    match result with
    | Error _ -> ()
    | Ok value -> failwith $"Expected error, got {value}"

let private dib = {
    Fn = FunctionField.Inst
    StNum = StorageNumber.zero
    Tariff = Tariff.zero
    SubUnit = SubUnit.zero
}

let private vib =
    parseExactly (parseField "VIB" InfoBlockRaw.parseVib) [| 0uy |]
    |> VibRsp.validate
    |> validationValue

let private record value : DataRecordRsp = { Dib = dib; Vib = vib; Value = value }

let private parseRecord bytes =
    parseExactly (parseField "Record" DataRecordRaw.parse) bytes
    |> DataRecordRsp.validate
    |> validationValue

[<Fact>]
let ``all value representations roundtrip through complete records`` () =
    // EN 13757-3:2025, 6.3.3, Tables 4-5 and Annex A.
    let values = [
        Value.NoData
        Value.Int8 -128y
        Value.Int16 -32768s
        Value.Int24 -8388608
        Value.Int24 8388607
        Value.Int32 Int32.MinValue
        Value.Int48 -140737488355328L
        Value.Int48 140737488355327L
        Value.Int64 Int64.MinValue
        Value.Real32 -12.5f
        Value.Bcd2Digit 99uy
        Value.Bcd4Digit 9999us
        Value.Bcd6Digit 999999u
        Value.Bcd8Digit 99999999u
        Value.Bcd12Digit 999999999999UL
        Value.Text "m³ ä"
        Value.Text ""
        Value.PosBcd 0UL
        Value.PosBcd 999999999999999999UL
        Value.NegBcd 0L
        Value.NegBcd -999999999999999999L
    ]

    for value in values do
        let decoded = record value |> DataRecordRsp.write |> written |> parseRecord
        decoded.Value |> should equal value

    for length in [ 0..15 ] @ [ 16; 20; 24; 28; 32; 48; 64 ] do
        let bytes = Array.init length byte

        match
            record (Value.Binary(ReadOnlyMemory bytes))
            |> DataRecordRsp.write
            |> written
            |> parseRecord
            |> _.Value
        with
        | Value.Binary actual -> actual.ToArray() |> should equal bytes
        | actual -> failwith $"Expected binary, got {actual}"

[<Fact>]
let ``invalid value representations fail without truncation`` () =
    for value in
        [
            Value.Int24 8388608
            Value.Int24 -8388609
            Value.Int48 140737488355328L
            Value.Bcd2Digit 100uy
            Value.Bcd4Digit 10000us
            Value.Bcd6Digit 1000000u
            Value.Bcd8Digit 100000000u
            Value.Bcd12Digit 1000000000000UL
            Value.PosBcd 1000000000000000000UL
            Value.NegBcd 1L
            Value.NegBcd Int64.MinValue
            Value.Text "€"
            Value.Text(String.replicate 192 "a")
            Value.Binary(ReadOnlyMemory(Array.zeroCreate<byte> 17))
        ] do
        Writer.run 261 (Value.write value) |> expectError

    Writer.run 261 (DataRecordRsp.write (record Value.SelectionForReadout))
    |> expectError

[<Fact>]
let ``record writer matches numeric and DIFE byte vectors`` () =
    // EN 13757-3:2025, Table 3, Table 8, Annex A.2.
    let extended = {
        dib with
            Fn = FunctionField.Max
            StNum = StorageNumber.create 33 |> Result.defaultWith failwith
            Tariff = Tariff.create 4 |> Result.defaultWith failwith
            SubUnit = SubUnit.create 2 |> Result.defaultWith failwith
    }

    let data = {
        record (Value.Int24 -2) with
            Dib = extended
    }

    let bytes = written (DataRecordRsp.write data)
    bytes |> should equal [| 0xD3uy; 0x80uy; 0x51uy; 0uy; 0xFEuy; 0xFFuy; 0xFFuy |]
    let decoded = parseRecord bytes
    decoded.Dib |> should equal extended
    decoded.Value |> should equal (Value.Int24 -2)

[<Fact>]
let ``text VIF is length-prefixed reversed ASCII and does not consume the value`` () =
    // EN 13757-3:2025, 6.4.1 and C.2.
    let data = {
        record (Value.Int8 7y) with
            Vib = VibRsp.Text "kWh"
    }

    let bytes = written (DataRecordRsp.write data)
    bytes |> should equal [| 1uy; 0x7Cuy; 3uy; byte 'h'; byte 'W'; byte 'k'; 7uy |]
    (parseRecord bytes).Vib |> should equal data.Vib
    (parseRecord bytes).Value |> should equal data.Value

[<Fact>]
let ``supported VIF table entries have a canonical reversible encoding`` () =
    // EN 13757-3:2025, Tables 10-15: aliases may canonicalize to another equivalent code.
    for prefix in [ [||]; [| 0xFDuy |]; [| 0xFBuy |]; [| 0xFDuy; 0xFDuy |] ] do
        for code in 0uy .. 0x7Auy do
            let bytes = Array.append prefix [| code |]
            let raw = parseExactly (parseField "VIB" InfoBlockRaw.parseVib) bytes

            match VibRsp.validate raw with
            | Passed(VibRsp.Normal normal, _) ->
                let encoded = VibRsp.write (VibRsp.Normal normal) |> written

                let decoded =
                    parseExactly (parseField "VIB" InfoBlockRaw.parseVib) encoded
                    |> VibRsp.validate
                    |> validationValue

                decoded |> should equal (VibRsp.Normal normal)
            | _ -> ()

[<Fact>]
let ``actions and record errors preserve their extension chains`` () =
    // EN 13757-3:2025, Tables 18-19; GET = E0001111, not FF.
    let definition =
        match vib with
        | VibRsp.Normal n -> n.Def
        | _ -> failwith "normal"

    let command =
        VibCmd.Normal {
            Def = definition
            Ext = []
            Actions = [ ActionCode.AddValue; ActionCode.Get ]
        }

    let bytes = VibCmd.write command |> written
    bytes |> should equal [| 0x80uy; 0x81uy; 0x0Fuy |]

    parseExactly (parseField "VIB" InfoBlockRaw.parseVib) bytes
    |> VibCmd.validate
    |> validationValue
    |> should equal command

    let response =
        VibRsp.Normal {
            Def = definition
            Ext = []
            Codes = [ RecordError.TooManyDifes; RecordError.DataOverflow ]
        }

    let encoded = VibRsp.write response |> written

    parseExactly (parseField "VIB" InfoBlockRaw.parseVib) encoded
    |> VibRsp.validate
    |> validationValue
    |> should equal response

let private address = AField.Configured(PrimAdr.tryCreate 1uy |> Option.get)

let private identity =
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
    | _ -> failwith "identity"

let private meter = DeviceIdentification.toMeterAddress identity

let private status = {
    ApplicationError = ApplicationError.NoError
    PowerLow = false
    PermanentError = false
    TemporaryError = false
    Bit5 = false
    Bit6 = false
    Bit7 = false
}

let private response: ResponseUserData = {
    Address = address
    Acd = false
    Dfc = false
    Status = status
    MeterAddress = meter
    UserData = [ record (Value.Int16 42s) ]
    MfrData = None
    MoreFollows = false
}

let private noKeys = {
    new IExternalSecurityContextResolver with
        member _.ResolveMode5Context _ = None
}

let private encode =
    Encoder.create EncodingOptions.Plain noKeys {
        AccessNumber = AccessNumber.create 9uy
    }

let private encoded message =
    encode message |> Result.defaultWith (fun e -> failwith e.Message)

let private primary message = Message.PrimaryStationMessage message
let private secondary message = Message.SecondaryStationMessage message

[<Fact>]
let ``primary message writers match normative control and address vectors`` () =
    // EN 13757-2, 5.7.3 and Annex F.1/F.2; EN 13757-7, 8.4, Table 44.
    encoded (primary (PrimaryStationMessage.LinkLayerReset { Address = AField.Broadcast }))
    |> should equal [| 0x10uy; 0x40uy; 0xFFuy; 0x3Fuy; 0x16uy |]

    encoded (
        primary (
            PrimaryStationMessage.RequestUserData {
                Address = AField.Configured(PrimAdr.tryCreate 1uy |> Option.get)
                Fcb = false
            }
        )
    )
    |> should equal [| 0x10uy; 0x5Buy; 1uy; 0x5Cuy; 0x16uy |]

    encoded (
        primary (
            PrimaryStationMessage.RequestAlarms {
                Address = AField.Diagnosis
                Fcb = true
            }
        )
    )
    |> should equal [| 0x10uy; 0x7Auy; 0xFEuy; 0x78uy; 0x16uy |]

    encoded (
        primary (
            PrimaryStationMessage.SelectionOfDevice {
                Selection = DeviceSelection.AllDevices
            }
        )
    )
    |> should equal [| 0x68uy; 3uy; 3uy; 0x68uy; 0x53uy; 0xFDuy; 0x52uy; 0xA2uy; 0x16uy |]

[<Fact>]
let ``response messages include their DLL address long header and application data`` () =
    let bytes = encoded (secondary (SecondaryStationMessage.ResponseUserData response))
    let dll = parseExactly DllCodec.parse bytes |> DllCodec.validate |> validationValue

    match dll.Value with
    | Dll.VariableLength frame ->
        frame.AField.Value |> should equal address

        let tpl =
            parseExactly
                TplCodec.parse
                ((LinkUserDataBytes.bytes frame.LinkUserData.Value).ToArray())
            |> TplCodec.validate
            |> validationValue

        match tpl.Value with
        | Tpl.LongHeader header ->
            header.Ci.Value |> should equal CiFieldTplLongHeader.Response

            header.AplData.Value.ToArray()
            |> parseExactly RecordsRaw.parse
            |> _.Value
            |> RspUdData.validate
            |> validationValue
            |> RspUdData.records
            |> List.map _.Value
            |> should equal [ Value.Int16 42s ]
        | _ -> failwith "Expected long header"
    | _ -> failwith "Expected long frame"

    let alarms = parseExactly Alarms.parse [| 0xA5uy |] |> _.Value

    let alarm: ResponseAlarms = {
        Address = address
        Acd = true
        Dfc = true
        Status = status
        MeterAddress = meter
        Alarms = alarms
    }

    let bytes = encoded (secondary (SecondaryStationMessage.ResponseAlarms alarm))
    bytes[4] |> should equal 0x38uy
    bytes[6] |> should equal 0x75uy
    bytes[19] |> should equal 0xA5uy

    parseExactly DllCodec.parse bytes
    |> DllCodec.validate
    |> validationValue
    |> ignore

[<Fact>]
let ``selection patterns preserve wildcard granularity when written`` () =
    let bytes = [| 0x52uy; 0x7Fuy; 0x56uy; 0xFFuy; 0x12uy; 0xFFuy; 0x1Euy; 0xFFuy; 7uy |]

    let selection =
        parseExactly SelectionCodec.parse bytes
        |> SelectionCodec.validate
        |> validationValue

    written (DeviceSelection.write selection.Value) |> should equal bytes

[<Fact>]
let ``FT12 maximum payload and overflow are enforced during composition`` () =
    // Long CI/header = 13 bytes, leaving 240 bytes for APL; MDH consumes one.
    let make length = {
        response with
            UserData = []
            MfrData = Some(MfrSpecificData.ofBytes (ReadOnlyMemory(Array.zeroCreate<byte> length)))
    }

    let bytes = encoded (secondary (SecondaryStationMessage.ResponseUserData(make 239)))
    bytes.Length |> should equal 261
    bytes[1] |> should equal 255uy

    encode (secondary (SecondaryStationMessage.ResponseUserData(make 240)))
    |> expectError

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(127)>]
[<InlineData(128)>]
[<InlineData(250)>]
let ``address assignment writes a SET record with an unsigned address`` (newAddress: int) =
    // EN 13757-3:2025, Tables 4/10: 01/7A, type C; 6.4.7: implicit SET.
    let next =
        if newAddress = 0 then
            PrimaryAddress.Unconfigured
        else
            PrimaryAddress.PrimAdr(PrimAdr.tryCreate (byte newAddress) |> Option.get)

    let bytes =
        encoded (
            primary (
                PrimaryStationMessage.ChangePrimaryAddress {
                    OldAddress = address
                    NewAddress = next
                }
            )
        )

    bytes
    |> should equal [|
        0x68uy
        6uy
        6uy
        0x68uy
        0x53uy
        1uy
        0x51uy
        1uy
        0x7Auy
        byte newAddress
        byte ((0x20 + newAddress) &&& 255)
        0x16uy
    |]

    let frame =
        parseExactly DllCodec.parse bytes |> DllCodec.validate |> validationValue

    match frame.Value with
    | Dll.VariableLength frame ->
        let tpl =
            parseExactly
                TplCodec.parse
                ((LinkUserDataBytes.bytes frame.LinkUserData.Value).ToArray())
            |> TplCodec.validate
            |> validationValue

        match tpl.Value with
        | Tpl.NoneHeader command ->
            command.Ci.Value |> should equal CiFieldTplNoneHeader.Command

            command.AplData.Value.ToArray()
            |> should equal [| 1uy; 0x7Auy; byte newAddress |]
        | _ -> failwith "Expected command without header"
    | _ -> failwith "Expected variable-length frame"

[<Fact>]
let ``configuration flags occupy the high byte independently of encrypted blocks`` () =
    // EN 13757-7:2018, Tables 27 and 31: S=13, A=14, B=15; N=4..7.
    for high in [ 0x20uy; 0x40uy; 0x80uy; 0xE0uy ] do
        for mode in [ 0uy; 5uy ] do
            let low = if mode = 5uy then 0xFBuy else 0x0Buy
            let bytes = [| low; high ||| mode |]
            let raw = parseExactly ConfigurationFieldRaw.parse bytes

            let assertFlags synchronized accessibility =
                synchronized
                |> should
                    equal
                    (if high &&& 0x20uy = 0uy then
                         Synchronization.NotSynchronized
                     else
                         Synchronization.Synchronized)

                accessibility
                |> should equal (Accessibility.ofConfigurationBits (uint16 high <<< 8))

            match raw.Value with
            | Mode0Raw bits ->
                let cnf = ConfigurationFieldMode0.validate bits |> validationValue
                assertFlags cnf.Synchronized cnf.Accessibility
                written (ConfigurationFieldMode0.write cnf) |> should equal bytes
            | Mode5Raw bits ->
                let cnf = ConfigurationFieldMode5.validate bits |> validationValue |> _.Value
                assertFlags cnf.Synchronized cnf.Accessibility
                NumberOfEncryptedBlocks.value cnf.NumOfEncryptedBlocks |> should equal 15
                written (ConfigurationFieldMode5.write cnf) |> should equal bytes

    NumberOfEncryptedBlocks.tryCreate -1 |> should equal None
    NumberOfEncryptedBlocks.tryCreate 16 |> should equal None

    NumberOfEncryptedBlocks.tryCreate 15
    |> Option.map NumberOfEncryptedBlocks.value
    |> should equal (Some 15)

[<Fact>]
let ``TPL and ELL writers preserve represented headers and opaque payload`` () =
    let payloads = [
        [| 0x51uy; 1uy; 0uy; 7uy |]
        [| 0x7Auy; 9uy; 0xA5uy; 0uy; 0xE0uy; 1uy; 0uy; 7uy |]
        [|
            0x72uy
            0x78uy
            0x56uy
            0x34uy
            0x12uy
            0xE6uy
            0x1Euy
            0x3Cuy
            7uy
            9uy
            0xA5uy
            0uy
            0xE0uy
            1uy
            0uy
            7uy
        |]
        // Mode 5 serialization preserves already represented protected bytes; it does not encrypt.
        [|
            0x72uy
            0x78uy
            0x56uy
            0x34uy
            0x12uy
            0xE6uy
            0x1Euy
            0x3Cuy
            7uy
            9uy
            0xA5uy
            0x10uy
            0xE5uy
            0xCCuy
        |]
    ]

    for bytes in payloads do
        let tpl = parseExactly TplCodec.parse bytes |> TplCodec.validate |> validationValue
        written (Tpl.write tpl.Value) |> should equal bytes

    let ellBytes = [| 0x8Cuy; 0xA5uy; 0x69uy |]

    let ell =
        parseExactly Metering.Mbus.Protocol.Frames.ExtendedLinkLayer.EllCodec.parse ellBytes
        |> Metering.Mbus.Protocol.Frames.ExtendedLinkLayer.EllCodec.validate
        |> validationValue

    written (Metering.Mbus.Protocol.Frames.ExtendedLinkLayer.Ell.write ell.Value)
    |> should equal ellBytes

[<Fact>]
let ``extended text VIF parses before its length and fails explicitly in the narrow text model``
    ()
    =
    // EN 13757-3:2025, C.2, Table C.3, complete normative record.
    let bytes = [|
        0x0Cuy
        0xFCuy
        0xA2uy
        0x73uy
        4uy
        0x6Cuy
        0x61uy
        0x67uy
        0x69uy
        0x26uy
        0x08uy
        0x42uy
        0x75uy
    |]

    let raw = parseExactly (parseField "Record" DataRecordRaw.parse) bytes
    raw.Value.Vib.Span.Length |> should equal 8

    Value.validate raw.Value.Value
    |> validationValue
    |> should equal (Value.Bcd8Digit 75420826u)

    match VibRsp.validate raw.Value.Vib with
    | Failed(failures, _) ->
        failures
        |> Failures.toList
        |> List.exists (fun issue -> issue.Message.Contains "Extensions on plain-text VIF")
        |> should be True
    | _ -> failwith "The narrow Text model must not silently drop VIFEs"

[<Fact>]
let ``DIFE and VIFE limits reject overflow and preserve the tenth extension`` () =
    // EN 13757-3:2025, 6.3.7 and 6.4.1: at most ten extension bytes.
    let raw =
        Array.concat [ [| 0x81uy |]; Array.create 9 0xFFuy; [| 0x7Fuy; 0uy; 1uy |] ]

    let data = parseRecord raw
    written (DataRecordRsp.write data) |> should equal raw

    let definition =
        match vib with
        | VibRsp.Normal n -> n.Def
        | _ -> failwith "normal"

    let extensions count =
        VibRsp.Normal {
            Def = definition
            Ext = List.replicate count (CombVifExt CombVifExt.AverageValue)
            Codes = []
        }

    let bytes = written (VibRsp.write (extensions 10))
    bytes.Length |> should equal 11

    parseExactly (parseField "VIB" InfoBlockRaw.parseVib) bytes
    |> VibRsp.validate
    |> validationValue
    |> should equal (extensions 10)

    Writer.run 261 (VibRsp.write (extensions 11)) |> expectError

    let terminal =
        VibRsp.Normal {
            Def = definition
            Ext = [ MfrVifExt ReadOnlyMemory<byte>.Empty ]
            Codes = [ RecordError.NoError ]
        }

    Writer.run 261 (VibRsp.write terminal) |> expectError

[<Fact>]
let ``response APL writes the final manufacturer block and more follows marker`` () =
    // EN 13757-3:2025, 6.6 and Table 6.
    for more in [ false; true ] do
        let payload =
            RspUdData.create
                []
                (Some(MfrSpecificData.ofBytes (ReadOnlyMemory [| 0xA5uy; 0xFFuy |])))
                more

        let bytes = RspUdData.write payload |> written
        bytes |> should equal [| (if more then 0x1Fuy else 0x0Fuy); 0xA5uy; 0xFFuy |]

        let decoded =
            parseExactly RecordsRaw.parse bytes
            |> _.Value
            |> RspUdData.validate
            |> validationValue

        RspUdData.moreFollows decoded |> should equal more

        RspUdData.mfrData decoded
        |> Option.map (MfrSpecificData.bytes >> _.ToArray())
        |> should equal (Some [| 0xA5uy; 0xFFuy |])

    RspUdData.create [] None true
    |> RspUdData.write
    |> written
    |> should equal [| 0x1Fuy |]
