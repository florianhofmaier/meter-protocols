using System.Text.Json;
using Metering.Analysis;
using Metering.Mbus.AnalysisAdapter;
using Xunit;

namespace Metering.AnalysisTests;

public sealed class InputTests
{
    [Theory]
    [InlineData("105B015C16")]
    [InlineData("10 5B 01 5C 16")]
    [InlineData("0x10, 0x5B, 0x01, 0x5C, 0x16")]
    [InlineData("\\x10\\x5b\\x01\\x5c\\x16")]
    [InlineData("[16,91,1,92,22]")]
    public void CommonByteRepresentationsPreserveTheBytes(string input)
    {
        Assert.Contains(InputInterpreter.Decode(input), x => Convert.ToHexString(x.Bytes) == "105B015C16");
    }

    [Fact]
    public void HexIsUnambiguousAndBase64IsNotAccepted()
    {
        Assert.Equal("AAAA", Convert.ToHexString(Assert.Single(InputInterpreter.Decode("AAAA")).Bytes));
        Assert.Single(InputInterpreter.Decode("hex: AAAA"));
        Assert.Empty(InputInterpreter.Decode("base64: AAAA"));
        Assert.Empty(InputInterpreter.Decode("EFsBXBY="));
    }

    [Theory]
    [InlineData("RX: 10 5B 01 5C 16")]
    [InlineData("10 5B 0 5C 16")]
    [InlineData("0x1Z")]
    [InlineData("[256, -1]")]
    public void InvalidOrDecoratedInputIsNotSilentlyStripped(string input) => Assert.Empty(InputInterpreter.Decode(input));

    [Fact]
    public void TextLimitIsCheckedBeforeParsing() => Assert.Throws<ArgumentException>(() => InputInterpreter.Decode(new string('A', InputInterpreter.MaxTextLength + 1)));
}

public sealed class ServiceTests
{
    private static AnalysisService Service() => new([new MbusProtocolAnalyzer()]);

    // EN 13757-2+A1:2024 §§5.7.3, 5.7.7, pp.19–21; IEC 60870-5-2 §3.2.
    [Theory]
    [InlineData("E5", "ACK")]
    [InlineData("10 40 01 41 16", "SND-NKE")]
    [InlineData("10 5A 01 5B 16", "REQ-UD1")]
    [InlineData("10 7A 01 7B 16", "REQ-UD1")]
    [InlineData("10 5B 01 5C 16", "REQ-UD2")]
    [InlineData("10 7B 01 7C 16", "REQ-UD2")]
    [InlineData("68 03 03 68 53 01 FF 53 16", "SND-UD")]
    [InlineData("68 03 03 68 73 01 FF 73 16", "SND-UD")]
    [InlineData("68 03 03 68 43 01 FF 43 16", "SND-UD2")]
    [InlineData("68 03 03 68 63 01 FF 63 16", "SND-UD2")]
    [InlineData("68 03 03 68 08 01 FF 08 16", "RSP-UD")]
    [InlineData("68 03 03 68 38 01 FF 38 16", "RSP-UD")]
    public void TelegramIdentitySurvivesUnsupportedApplicationOrMessageMapping(string input, string name)
    {
        var report = Assert.Single(Service().Analyze("hex: " + input).Candidates).Report;
        Assert.NotNull(report.Message);
        Assert.Equal(name, report.Message.Name);
        var field = Assert.Single(Flatten(report.Fields), f => f.Id == report.Message.FieldId);
        Assert.IsType<ParsedField>(field.Observation);
        if (name != "ACK") Assert.Equal("CField", field.Name);
        Assert.Contains(report.Steps, s => s.Name == "Telegram.identify" && s.Outcome is SucceededStep);
        if (name == "SND-UD") Assert.False(report.AllStepsSucceeded);
    }

    [Theory]
    [InlineData("10 50 01 51 16")]
    [InlineData("10 60 01 61 16")]
    [InlineData("10 4A 01 4B 16")]
    [InlineData("10 4B 01 4C 16")]
    [InlineData("10 53 01 54 16")]
    [InlineData("10 08 01 09 16")]
    [InlineData("68 03 03 68 5B 01 FF 5B 16")]
    [InlineData("68 03 03 68 44 01 FF 44 16")]
    public void UnsupportedFrameFormOrControlFlagsDoNotInventAnIdentity(string input)
    {
        Assert.Null(Assert.Single(Service().Analyze("hex: " + input).Candidates).Report.Message);
    }

    // EN 13757-7 §7.4 Table 12; EN 13757-3 §§6.2–6.4 Tables 2/4/10.
    [Fact]
    public void ValidatedRecordComponentsArePresentedAtTheirActualTraceFields()
    {
        var report = Assert.Single(Service().Analyze("68 15 15 68 08 01 72 78 56 34 12 2D 2C 01 07 08 00 00 00 04 13 39 30 00 00 78 16").Candidates).Report;
        Assert.True(report.AllStepsSucceeded);
        var fields = Flatten(report.Fields).ToArray();
        Assert.Contains(Assert.Single(fields, f => f.Name == "VIB").Values, v => v.Text == "CubicMeters");
        Assert.Contains(Assert.Single(fields, f => f.Name == "VIB").Values, v => v.Text == "0.001");
        Assert.Contains(Assert.Single(fields, f => f.Name == "32 Bit Integer").Values, v => v.Text == "12345");
    }

    // EN 13757-2 Annex F.2, p.37: REQ-UD2 fixture. IEC 60870-5-2 §3.2, p.14: framing.
    [Fact]
    public void RealAnalyzerProducesLinkedFieldsAndExactSourceBytes()
    {
        var response = Service().Analyze("10 5B 01 5C 16");
        var candidate = Assert.Single(response.Candidates);
        Assert.Equal(candidate.Id, response.SelectedCandidateId);
        Assert.Equal("high", candidate.Confidence);
        Assert.Equal("mbus.wired", candidate.Report.Protocol.Id);
        Assert.Equal("105B015C16", Assert.Single(candidate.Report.Sources).Hex);
        Assert.Contains(Flatten(candidate.Report.Fields), f => f.Name == "PrimaryAddress" && f.Observation is ParsedField { Span.Offset: 2, Span.Length: 1 });
        Assert.Contains(candidate.Report.Steps, s => s.Name == "DLL.validate" && s.Outcome is SucceededStep);
        var json = JsonSerializer.Serialize(response, new JsonSerializerOptions(JsonSerializerDefaults.Web));
        using var document = JsonDocument.Parse(json);
        Assert.Equal("parsed", document.RootElement.GetProperty("candidates")[0].GetProperty("report").GetProperty("fields")[0].GetProperty("observation").GetProperty("kind").GetString());
    }

    [Fact]
    public void ChecksumFailureRemainsARecognizedButInvalidInterpretation()
    {
        var candidate = Assert.Single(Service().Analyze("10 5B 01 5D 16").Candidates);
        Assert.Equal("medium", candidate.Confidence);
        Assert.False(candidate.Report.AllStepsSucceeded);
        Assert.Equal("REQ-UD2", candidate.Report.Message?.Name);
        var diagnostic = Assert.Single(candidate.Report.Diagnostics, d => d.Kind == "validation");
        Assert.NotNull(diagnostic.FieldId);
        Assert.Equal(3, diagnostic.Span?.Offset);
        Assert.Equal(1, diagnostic.Span?.Length);
    }

    [Fact]
    public void TruncationKeepsSuccessfulChildrenWithoutInventingAFailedFieldLength()
    {
        var candidate = Assert.Single(Service().Analyze("10 5B").Candidates);
        var fields = Flatten(candidate.Report.Fields).ToArray();
        Assert.Contains(fields, f => f.Name == "CField" && f.Observation is ParsedField);
        Assert.Contains(fields, f => f.Observation is IncompleteField);
        Assert.Contains(candidate.Report.Steps, s => s.Outcome is BlockedStep);
        Assert.Equal("low", candidate.Confidence);
    }

    [Fact]
    public void UnsupportedBytesAreNotForcedIntoMbus()
    {
        var result = Service().Analyze("FF 00 01");
        Assert.Single(result.Inputs);
        Assert.Empty(result.Candidates);
        Assert.Null(result.SelectedCandidateId);
    }

    [Fact]
    public void RankingUsesAnalyzerEvidenceAcrossProtocolCandidates()
    {
        var service = new AnalysisService([new FakeAnalyzer("weaker", 30), new FakeAnalyzer("stronger", 90)]);
        var result = service.Analyze("00 00 00");
        Assert.Single(result.Inputs);
        Assert.Equal(2, result.Candidates.Count);
        Assert.Equal("stronger", result.Candidates[0].Report.Protocol.Id);
        Assert.Equal(result.Candidates[0].Id, result.SelectedCandidateId);
    }

    private static IEnumerable<AnalysisField> Flatten(IEnumerable<AnalysisField> fields) => fields.SelectMany(f => new[] { f }.Concat(Flatten(f.Children)));

    private sealed class FakeAnalyzer(string id, int rank) : IProtocolAnalyzer
    {
        public ProtocolDescriptor Descriptor { get; } = new(id, "Future protocol", "Test entry point");
        public ProtocolMatch? TryAnalyze(ReadOnlyMemory<byte> bytes) => bytes.Span.SequenceEqual(new byte[] { 0, 0, 0 })
            ? new(rank, "high", ["Fixture matched"], new(1, Descriptor, true, [], [], [], [])) : null;
    }
}
