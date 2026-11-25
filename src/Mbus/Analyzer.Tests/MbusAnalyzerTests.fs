module Metering.Mbus.Analysis.Tests.MbusAnalyzerTests

open System
open System.Text.Json
open System.Xml.Linq
open Xunit
open FsUnit.Xunit
open Metering.Mbus.Analysis
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Records
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type private Keys(key: byte[] option) =
    let mutable calls = 0
    member _.Calls = calls

    interface IExternalSecurityContextResolver with
        member _.ResolveMode5Context(_: MeterAddress) =
            calls <- calls + 1

            key
            |> Option.map (fun bytes ->
                Mode5ExternalContext(Secret128.TakeOwnership(Array.copy bytes)))

let private analyze (keys: Keys) bytes =
    MbusAnalyzer.analyze keys (ReadOnlyMemory<byte> bytes)

let private sourceBytes (report: MbusAnalysis) sourceId =
    report.Sources |> List.find (fun source -> source.Info.Id = sourceId) |> _.Bytes

let private step name (report: MbusAnalysis) =
    report.Steps |> List.find (fun step -> step.Name = name)

let private succeeded name report =
    match (step name report).Status with
    | AnalysisStepStatus.Succeeded -> true
    | _ -> false

let private fields (report: MbusAnalysis) =
    let rec flatten (field: ParseField) =
        field :: (field.Children |> List.collect flatten)

    report.Trace.Fields |> List.collect flatten

let private header configuration = [|
    0x72uy
    0x78uy
    0x56uy
    0x34uy
    0x12uy
    0x2Duy
    0x2Cuy
    0x01uy
    0x07uy
    0x08uy
    0x00uy
    byte configuration
    byte (configuration >>> 8)
|]

let private wired payload =
    let data = Array.append [| 0x08uy; 0x01uy |] payload
    let checksum = data |> Array.sumBy int |> byte

    Array.concat [
        [| 0x68uy; byte data.Length; byte data.Length; 0x68uy |]
        data
        [| checksum; 0x16uy |]
    ]

let private response = wired (Array.append (header 0x0000) [| 0x2Fuy |])

let private encrypt key (plain: byte[]) =
    // EN 13757-7:2018, 9.4.4.1, Table 48: independent IV fixture.
    let iv = [|
        0x2Duy
        0x2Cuy
        0x78uy
        0x56uy
        0x34uy
        0x12uy
        0x01uy
        0x07uy
        0x08uy
        0x08uy
        0x08uy
        0x08uy
        0x08uy
        0x08uy
        0x08uy
        0x08uy
    |]

    use aes = System.Security.Cryptography.Aes.Create()
    aes.Mode <- System.Security.Cryptography.CipherMode.CBC
    aes.Padding <- System.Security.Cryptography.PaddingMode.None
    use transform = aes.CreateEncryptor(key, iv)
    transform.TransformFinalBlock(plain, 0, plain.Length)

[<Fact>]
let ``valid analysis links validated values to original fields and omits absent layers`` () =
    let keys = Keys(None)
    let report = analyze keys response
    report.IsValid |> should be True
    keys.Calls |> should equal 0
    succeeded "Message.validate" report |> should be True

    report.Steps
    |> List.exists (fun step ->
        step.Name.StartsWith("ELL")
        || step.Name.StartsWith("NWL")
        || step.Name.StartsWith("AFL"))
    |> should be False

    let tpl = fields report |> List.find (fun field -> field.Name = "TPL")

    match step "TPL.validate" report, tpl.Result with
    | MbusAnalysisStep.TplValidation {
                                         Result = AnalysisStepResult.Succeeded value
                                     },
      ParseFieldResult.Parsed span ->
        value.Id |> should equal tpl.Id
        value.Span |> should equal span
    | actual -> failwith $"Expected linked validated TPL, got {actual}"

    let ids = fields report |> List.map _.Id
    ids |> Set.ofList |> Set.count |> should equal ids.Length

    for item in report.Steps do
        item.Metadata.Dependencies
        |> List.iter (fun cause -> Assert.True(cause < item.Metadata.Id))

[<Fact>]
let ``invalid DLL checksum does not prevent bounded higher layer analysis`` () =
    // IEC 60870-5-2, 3.2: DLL user-data boundary is independent of checksum validation.
    let bytes = Array.copy response
    bytes[bytes.Length - 2] <- bytes[bytes.Length - 2] ^^^ 1uy
    let report = analyze (Keys(None)) bytes
    report.IsValid |> should be False

    match (step "DLL.validate" report).Status with
    | AnalysisStepStatus.Failed(ValidationFailed failures) ->
        let knownIds = fields report |> List.map _.Id |> Set.ofList

        Failures.toList failures
        |> List.iter (fun issue -> Assert.True(knownIds.Contains issue.FieldId))
    | actual -> failwith $"Expected checksum diagnostic, got {actual}"

    succeeded "TPL.validate" report |> should be True
    succeeded "APL.validate" report |> should be True

    match (step "Message.validate" report).Status with
    | AnalysisStepStatus.Blocked(cause, _) ->
        cause |> should equal (step "DLL.validate" report).Metadata.Id
    | actual -> failwith $"Expected message mapping blocked by DLL validation, got {actual}"

[<Fact>]
let ``TPL accumulates independent issues while plaintext APL remains reachable`` () =
    // EN 13757-7, 7.5.1 and 7.7.1 Tables 21/22: invalid BCD and reserved CC.
    let bytes = header 0x0004
    bytes[4] <- 0xAAuy
    let report = analyze (Keys(None)) (wired (Array.append bytes [| 0x2Fuy |]))

    match (step "TPL.validate" report).Status with
    | AnalysisStepStatus.Failed(ValidationFailed failures) ->
        Failures.toList failures |> should haveLength 2

        Failures.toList failures
        |> List.map _.FieldId
        |> Set.ofList
        |> Set.count
        |> should equal 2
    | actual -> failwith $"Expected independent TPL diagnostics, got {actual}"

    succeeded "APL.validate" report |> should be True
    report.IsValid |> should be False

[<Fact>]
let ``truncated TPL retains completed children and blocks dependent parsing`` () =
    // EN 13757-7, 7.4, Table 12: last configuration byte missing.
    let bytes = header 0x0000 |> Array.take 12 |> wired
    let report = analyze (Keys(None)) bytes

    match (step "TPL.parse" report).Status with
    | AnalysisStepStatus.Failed(ParseFailed _) -> ()
    | actual -> failwith $"Expected truncated TPL, got {actual}"

    let access = fields report |> List.find (fun field -> field.Name = "Access Number")

    match access.Result with
    | ParseFieldResult.Parsed span ->
        let source = sourceBytes report span.Source
        source.Span[span.Offset] |> should equal 8uy
    | actual -> failwith $"Expected completed access-number span, got {actual}"

    match (step "APL.parse" report).Status with
    | AnalysisStepStatus.Blocked(cause, _) ->
        cause |> should equal (step "TPL.parse" report).Metadata.Id
    | actual -> failwith $"Expected blocked APL, got {actual}"

    fields report
    |> List.exists (fun field -> field.Name = "APL")
    |> should be False

[<Theory>]
[<InlineData(0x81)>]
[<InlineData(0x90)>]
[<InlineData(0xFF)>]
let ``unsupported or unknown layers remain errors with no invented TPL`` ci =
    let report = analyze (Keys(None)) (wired [| byte ci |])
    report.IsValid |> should be False

    report.Steps
    |> List.exists (fun step ->
        match step.Status with
        | AnalysisStepStatus.Failed(ParseFailed _) -> true
        | _ -> false)
    |> should be True

    fields report
    |> List.exists (fun field -> field.Name = "TPL" || field.Name = "APL")
    |> should be False

[<Fact>]
let ``present ELL is independently parsed and validated`` () =
    // EN 13757-4, 13.2.2, Table 44: CI 8C, CC, ACC.
    let payload = Array.concat [ [| 0x8Cuy; 0x00uy; 0x08uy |]; header 0; [| 0x2Fuy |] ]
    let report = analyze (Keys(None)) (wired payload)
    report.IsValid |> should be True
    succeeded "ELL.validate" report |> should be True
    succeeded "Message.validate" report |> should be True

    let ell =
        fields report |> List.find (fun field -> field.Name = "Extended Link Layer")

    let tpl = fields report |> List.find (fun field -> field.Name = "TPL")

    match ell.Result, tpl.Result with
    | ParseFieldResult.Parsed ellSpan, ParseFieldResult.Parsed tplSpan ->
        ellSpan.Offset |> should equal 6
        ellSpan.Length |> should equal 3
        tplSpan.Offset |> should equal 9
    | actual -> failwith $"Expected separate ELL and TPL spans, got {actual}"

    ell.Children
    |> List.exists (fun child -> child.Name = "ELL payload")
    |> should be False

    fields report
    |> List.exists (fun field -> field.Name = "Following layer bytes")
    |> should be False

[<Fact>]
let ``missing key preserves protected bytes and blocks APL with the actual cause`` () =
    let keys = Keys(None)
    let bytes = wired (Array.append (header 0x0510) (Array.zeroCreate 16))
    let report = analyze keys bytes
    keys.Calls |> should equal 1
    report.Trace.Sources |> should haveLength 1

    (sourceBytes report report.Trace.Sources.Head.Id).ToArray()
    |> should equal bytes

    match (step "Protection.unprotect" report).Status, (step "APL.parse" report).Status with
    | AnalysisStepStatus.Failed(EncryptionFailed _), AnalysisStepStatus.Blocked(cause, _) ->
        cause |> should equal (step "Protection.unprotect" report).Metadata.Id
    | actual -> failwith $"Expected missing key and dependent blocker, got {actual}"

[<Theory>]
[<InlineData(0x0510, 15)>]
[<InlineData(0x05F0, 17)>]
let ``invalid cipher boundaries fail before key lookup`` configuration length =
    let keys = Keys(None)

    let report =
        analyze keys (wired (Array.append (header configuration) (Array.zeroCreate length)))

    keys.Calls |> should equal 0

    match (step "Protection.prepare" report).Status with
    | AnalysisStepStatus.Failed(EncryptionFailed _) -> ()
    | actual -> failwith $"Expected protection layout error, got {actual}"

    report.Trace.Sources |> should haveLength 1

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``Mode 5 uses concrete prerequisites rather than whole TPL validity`` invalidContent =
    let key = Array.init 16 byte
    let keys = Keys(Some key)
    let configuration = if invalidContent then 0x0514 else 0x0510
    let plain = Array.create 16 0x2Fuy
    let clear = [| 0x2Fuy |]

    let report =
        analyze keys (wired (Array.concat [ header configuration; encrypt key plain; clear ]))

    keys.Calls |> should equal 1
    succeeded "APL.validate" report |> should be True
    report.IsValid |> should equal (not invalidContent)
    report.Trace.Sources |> should haveLength 2
    let derived = report.Trace.Sources |> List.find (fun source -> source.Origin.IsSome)
    derived.Sensitive |> should be True

    (sourceBytes report derived.Id).ToArray()
    |> should equal (Array.append plain clear)

    let original = fields report |> List.find (fun field -> field.Name = "APL Data")

    let decrypted =
        fields report |> List.find (fun field -> field.Name = "Decrypted APL Data")

    original.Id |> should not' (equal decrypted.Id)

    match original.Result with
    | ParseFieldResult.Parsed span -> derived.Origin |> should equal (Some span)
    | actual -> failwith $"Expected original encrypted data span, got {actual}"

    let json = MbusAnalysis.toJson report
    json.Contains(Convert.ToHexString key) |> should be False

[<Fact>]
let ``wrong key does not register plaintext or parse APL`` () =
    let key = Array.init 16 byte
    let keys = Keys(Some(Array.create 16 0xAAuy))
    let cipher = encrypt key (Array.create 16 0x2Fuy)
    let report = analyze keys (wired (Array.append (header 0x0510) cipher))
    report.Trace.Sources |> should haveLength 1

    fields report
    |> List.exists (fun field -> field.Name = "APL")
    |> should be False

    match (step "Protection.unprotect" report).Status with
    | AnalysisStepStatus.Failed(EncryptionFailed issue) ->
        Assert.Contains("verification", issue.Message)
    | actual -> failwith $"Expected verification failure, got {actual}"

[<Theory>]
[<InlineData(0x0510, false)>]
[<InlineData(0x0500, true)>]
let ``invalid identity blocks only operations that actually require it``
    configuration
    aplReachable
    =
    let keys = Keys(None)
    let bytes = header configuration
    bytes[4] <- 0xAAuy
    let payload = if aplReachable then [| 0x2Fuy |] else Array.zeroCreate 16
    let report = analyze keys (wired (Array.append bytes payload))
    report.IsValid |> should be False
    keys.Calls |> should equal 0
    succeeded "Protection.prepare" report |> should equal aplReachable

    if aplReachable then
        succeeded "APL.validate" report |> should be True
    else
        match (step "Protection.prepare" report).Status with
        | AnalysisStepStatus.Failed(ValidationFailed _) -> ()
        | actual -> failwith $"Expected failed protection prerequisites, got {actual}"

[<Fact>]
let ``JSON and XML export preserve source bytes diagnostics and validated fields`` () =
    let report = analyze (Keys(None)) response
    let json = MbusAnalysis.toJson report
    use parsed = JsonDocument.Parse json
    parsed.RootElement.GetProperty("isValid").GetBoolean() |> should be True

    parsed.RootElement.GetProperty("sources").[0].GetProperty("bytes").GetString()
    |> should equal (Convert.ToHexString response)

    let xml = XDocument.Parse(MbusAnalysis.toXml report)
    xml.Root.Name.LocalName |> should equal "mbusAnalysis"

    xml.Root.Element(XName.Get "steps").Elements(XName.Get "item")
    |> Seq.length
    |> should equal report.Steps.Length

    xml.Root.Element(XName.Get "sources").Value.Contains(Convert.ToHexString response)
    |> should be True

    let again = MbusAnalysis.toJson report
    again |> should equal json

[<Fact>]
let ``empty input produces an exportable parse failure without dependent decoding`` () =
    let report = analyze (Keys(None)) [||]
    report.IsValid |> should be False

    match (step "DLL.parse" report).Status with
    | AnalysisStepStatus.Failed(ParseFailed error) ->
        error.Source |> should equal report.Trace.Sources.Head.Id
    | actual -> failwith $"Expected empty input error, got {actual}"

    Assert.Contains("blocked", MbusAnalysis.toJson report)

[<Fact>]
let ``trailing bytes are reported even after a complete DLL field`` () =
    let report = analyze (Keys(None)) (Array.append response [| 0x00uy |])

    match (step "DLL.parse" report).Status with
    | AnalysisStepStatus.Failed(ParseFailed error) ->
        error.Pos |> should equal response.Length
        Assert.Contains("trailing", error.Msg)
    | actual -> failwith $"Expected exact-input failure, got {actual}"

    fields report
    |> List.exists (fun field ->
        match field.Result with
        | ParseFieldResult.Parsed span -> span.Offset = 0 && span.Length = response.Length
        | _ -> false)
    |> should be True

    report.Steps
    |> List.exists (fun step -> step.Name = "TPL.parse")
    |> should be False

[<Fact>]
let ``fixed frame is analyzed without higher layers`` () =
    // EN 13757-2:2024, Annex F.2, p. 37: fixed-length REQ-UD2 example.
    let report = analyze (Keys(None)) [| 0x10uy; 0x5Buy; 0x01uy; 0x5Cuy; 0x16uy |]
    report.IsValid |> should be True

    report.Steps
    |> List.map _.Name
    |> should equal [ "DLL.parse"; "DLL.validate"; "Telegram.identify"; "Message.validate" ]

[<Fact>]
let ``failure export preserves the diagnostic field reference and blocking cause`` () =
    let bytes = Array.copy response
    bytes[bytes.Length - 2] <- bytes[bytes.Length - 2] ^^^ 1uy
    let report = analyze (Keys(None)) bytes
    use json = JsonDocument.Parse(MbusAnalysis.toJson report)
    let steps = json.RootElement.GetProperty("steps").EnumerateArray() |> Seq.toArray

    let dll =
        steps
        |> Array.find (fun step -> step.GetProperty("name").GetString() = "DLL.validate")

    let diagnostic = dll.GetProperty("result").GetProperty("diagnostic")
    let issueId = diagnostic.GetProperty("issues").[0].GetProperty("fieldId").GetInt32()

    fields report
    |> List.exists (fun field -> FieldId.value field.Id = issueId)
    |> should be True

    let message =
        steps
        |> Array.find (fun step -> step.GetProperty("name").GetString() = "Message.validate")

    message.GetProperty("result").GetProperty("cause").GetInt32()
    |> should equal (step "DLL.validate" report).Metadata.Id.Value

    let xml = XDocument.Parse(MbusAnalysis.toXml report)

    xml.Descendants(XName.Get "fieldId")
    |> Seq.exists (fun node -> node.Value = string issueId)
    |> should be True

[<Fact>]
let ``APL record values including floating point and control text remain exportable`` () =
    // EN 13757-3:2025, 6.3.3 Tables 4/5, 6.4.2 Table 10, Annex A Table A.7.
    let records = [|
        0x01uy
        0x03uy
        0x7Buy // signed int8, energy in Wh
        0x05uy
        0x03uy
        0uy
        0uy
        0x80uy
        0x7Fuy // positive infinity, binary32
        0x0Duy
        0x03uy
        0x01uy
        0x00uy // one Latin-1 character, NUL
    |]

    let report = analyze (Keys(None)) (wired (Array.append (header 0) records))
    succeeded "APL.validate" report |> should be True

    match step "APL.validate" report with
    | MbusAnalysisStep.AplValidation {
                                         Result = AnalysisStepResult.Succeeded value
                                     } ->
        match value.Value with
        | Apl.RspUdData data ->
            RspUdData.records data
            |> List.map _.Value
            |> should equal [
                Value.Int8 123y
                Value.Real32 Single.PositiveInfinity
                Value.Text "\000"
            ]
        | actual -> failwith $"Expected typed response records, got {actual}"
    | actual -> failwith $"Expected typed APL value, got {actual}"

    let exported = MbusAnalysis.toJson report

    for expected in [ "Int8"; "123"; "WattHours"; "Infinity"; "\\u0000" ] do
        Assert.Contains(expected, exported)

    use json = JsonDocument.Parse exported
    let xml = XDocument.Parse(MbusAnalysis.toXml report)

    let escaped =
        xml.Descendants()
        |> Seq.find (fun node ->
            let encoding = node.Attribute(XName.Get "encoding")
            not (isNull encoding) && encoding.Value = "json")

    JsonSerializer.Deserialize<string>(escaped.Value) |> should equal "\000"
