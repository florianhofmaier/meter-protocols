using System.Collections;
using System.Globalization;
using System.Reflection;
using System.Text.Json;
using Microsoft.FSharp.Reflection;
using Metering.Common.Decoding.Parsers;
using Metering.Common.Decoding.Parsers.Types;
using Metering.Mbus.Protocol.Frames.ApplicationLayer;
using Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus;
using DecodeFailure = Metering.Common.Decoding.Decoders.Core.DecodeFailure;
using Notice = Metering.Common.Decoding.Validators.Core.Notice;
using Issue = Metering.Common.Decoding.Validators.Core.Issue;
using FailuresModule = Metering.Common.Decoding.Validators.Core.FailuresModule;
using NativeSpan = Metering.Common.Decoding.Parsers.Types.SourceSpan;
using SourceSpan = Metering.Analysis.SourceSpan;
using Metering.Analysis;
using Metering.Mbus.Analysis;
using Metering.Mbus.Protocol.Frames.DeviceIdentification;
using Metering.Mbus.Protocol.Frames.TransportLayer.Security;
using Microsoft.FSharp.Core;
using AnalysisStep = Metering.Analysis.AnalysisStep;
using AnalysisSource = Metering.Analysis.AnalysisSource;

namespace Metering.Mbus.AnalysisAdapter;

/// <summary>Projects the existing wired analyzer into the UI-independent report contract.</summary>
public sealed class MbusProtocolAnalyzer : IProtocolAnalyzer
{
    public ProtocolDescriptor Descriptor { get; } = new("mbus.wired", "M-Bus", "Ein einzelnes wired FT-1.2-Telegramm");

    public ProtocolMatch? TryAnalyze(ReadOnlyMemory<byte> bytes)
    {
        // IEC 60870-5-2 §3.2, p.14 (PDF p.16); EN 13757-2 Annex F.1/F.2, p.37.
        // A start-byte match is only a hypothesis. All validation remains in MbusAnalyzer.
        if (bytes.IsEmpty || bytes.Span[0] is not (0xE5 or 0x10 or 0x68)) return null;
        var native = MbusAnalyzer.analyze(new NoKeys(), bytes);
        var mapper = new ReportMapper(Descriptor, native);
        var report = mapper.Map();
        bool Succeeded(string name) => report.Steps.Any(s => s.Name == name && s.Outcome is SucceededStep);
        var parsed = Succeeded("DLL.parse");
        var validated = Succeeded("DLL.validate");
        var evidence = new List<string> { $"Startbyte {bytes.Span[0]:X2} passt zum implementierten wired FT-1.2-Einstieg." };
        evidence.Add(parsed ? "Der DLL-Parser hat die vollständige Eingabe strukturell gelesen." : "Die Struktur ist unvollständig oder beschädigt; Details stehen in den Diagnosen.");
        if (validated) evidence.Add("Die DLL-Validierung war erfolgreich.");
        if (!report.AllStepsSucceeded) evidence.Add("Mindestens ein Analyseschritt ist fehlgeschlagen oder blockiert.");
        return new(validated ? 90 : parsed ? 65 : 30, validated ? "high" : parsed ? "medium" : "low", evidence, report);
    }

    private sealed class NoKeys : IExternalSecurityContextResolver
    {
        public FSharpOption<Mode5ExternalContext> ResolveMode5Context(MeterAddress meterIdentity) => null!;
    }
}

internal sealed class ReportMapper(ProtocolDescriptor protocol, MbusAnalysis native)
{
    private readonly Dictionary<string, List<FieldValue>> values = [];
    private readonly Dictionary<string, SourceSpan> spans = [];
    private readonly List<AnalysisDiagnostic> diagnostics = [];

    private static string Id(int id) => id.ToString(CultureInfo.InvariantCulture);
    private static string Id(AnalysisStepId id) => Id(id.Value);
    private static string Id(FieldId id) => Id(FieldIdModule.value(id));
    private static SourceSpan Span(NativeSpan span) => new(Id(span.Source.Value), span.Offset, span.Length);

    public AnalysisReport Map()
    {
        foreach (var step in native.Steps) CollectValues(PresentationValues.Value(step));
        IndexRecordValues();
        var fields = native.Trace.Fields.Select(MapField).ToArray();
        var steps = native.Steps.Select(MapStep).ToArray();
        var sources = native.Sources.Select(source => new AnalysisSource(
            Id(source.Info.Id.Value), source.Info.Name, Convert.ToHexString(source.Bytes.Span), source.Info.Sensitive,
            source.Info.Origin is { } origin ? Span(origin.Value) : null)).ToArray();
        return new(1, protocol, native.IsValid, sources, fields, diagnostics, steps) { Message = MapMessage() };
    }

    private MessageIdentity? MapMessage()
    {
        var identified = native.Steps.OfType<MbusAnalysisStep.TelegramIdentification>()
            .Select(s => s.Item.Result).OfType<AnalysisStepResult<Field<TelegramType>>.Succeeded>().FirstOrDefault();
        if (identified is null) return null;
        var field = identified.Item;
        // The typed core classification supplies identity; the adapter only adds presentation text.
        var details = field.Value switch
        {
            var t when t.IsAck => ("ACK", "Bestätigung", false),
            var t when t.IsSndNke => ("SND-NKE", "Verbindungsinitialisierung", true),
            var t when t.IsReqUd1 => ("REQ-UD1", "Zeitkritische Daten anfordern", true),
            var t when t.IsReqUd2 => ("REQ-UD2", "Daten anfordern", true),
            var t when t.IsSndUd => ("SND-UD", "Daten senden · Bestätigung angefordert", true),
            var t when t.IsSndUd2 => ("SND-UD2", "Daten senden · Datenantwort angefordert", true),
            var t when t.IsRspUd => ("RSP-UD", "Datenantwort", false),
            _ => throw new InvalidOperationException("Unknown telegram classification.")
        };
        return new(details.Item1, details.Item2,
            details.Item3 ? "Master → Zähler" : "Zähler → Master", Id(field.Id),
            "EN 13757-2+A1:2024 §5.7.3, §5.7.7; IEC 60870-5-2 §3.2");
    }

    private void CollectValues(object? value)
    {
        if (value is null) return;
        if (PresentationValues.IsField(value))
        {
            var type = value.GetType();
            var id = Id((FieldId)type.GetProperty("Id")!.GetValue(value)!);
            if (!values.TryGetValue(id, out var list)) values[id] = list = [];
            foreach (var item in PresentationValues.Flatten(type.GetProperty("Value")!.GetValue(value)))
                if (!list.Contains(item)) list.Add(item);
        }
        foreach (var (_, child) in PresentationValues.Members(value)) CollectValues(child);
    }

    private sealed record RecordPresentation(object Whole, object Dib, object Vib, object Value);

    private void IndexRecordValues()
    {
        // EN 13757-3 §6.2, Table 2: associate validated DIB/VIB/data with existing trace fields.
        static IEnumerable<ParseField> TraceFields(ParseField field) =>
            new[] { field }.Concat(field.Children.SelectMany(TraceFields));
        var apl = native.Steps.OfType<MbusAnalysisStep.AplValidation>()
            .Select(s => s.Item.Result).OfType<AnalysisStepResult<Field<Apl>>.Succeeded>().FirstOrDefault();
        var records = apl?.Item.Value switch
        {
            Apl.RspUdData response => RspUdDataModule.records(response.Item)
                .Select(r => new RecordPresentation(r, r.Dib, r.Vib, r.Value)).ToArray(),
            Apl.SndUdData command => SndUdDataModule.records(command.Item)
                .Select(r => new RecordPresentation(r, r.Dib, r.Vib, r.Value)).ToArray(),
            _ => []
        };
        var trace = native.Trace.Fields.SelectMany(TraceFields).Where(f => f.Name == "Data Record").ToArray();
        if (records.Length != trace.Length) return; // Do not infer associations for a different trace shape.
        void Present(ParseField field, object value) => values[Id(field.Id)] = PresentationValues.Flatten(value).ToList();
        for (var i = 0; i < records.Length; i++)
        {
            Present(trace[i], records[i].Whole);
            foreach (var child in trace[i].Children)
            {
                var content = child.Name switch { "DIB" => records[i].Dib, "VIB" => records[i].Vib, "Data" => records[i].Value, _ => null };
                if (content is null) continue;
                Present(child, content);
                if (child.Name == "Data" && child.Result is ParseFieldResult.Parsed childSpan)
                    foreach (var nested in child.Children)
                        if (nested.Result is ParseFieldResult.Parsed nestedSpan && nestedSpan.span.Equals(childSpan.span))
                            Present(nested, content);
            }
        }
    }

    private AnalysisField MapField(ParseField field)
    {
        var id = Id(field.Id);
        FieldObservation observation;
        if (field.Result is ParseFieldResult.Parsed parsed)
        {
            var span = Span(parsed.span);
            spans[id] = span;
            observation = new ParsedField(span);
        }
        else
        {
            var (start, reason) = field.Result switch
            {
                ParseFieldResult.Failed failed => (failed.start, failed.error.Msg),
                ParseFieldResult.Reading reading => (reading.start, ""),
                _ => throw new InvalidOperationException("Unknown field observation.")
            };
            spans[id] = Span(start) with { Length = 0 };
            observation = new IncompleteField(Id(start.Source.Value), start.Offset, reason);
        }
        return new(id, field.Name, observation,
            values.TryGetValue(id, out var presented) ? presented : [],
            MbusFieldDocumentation.Describe(field.Name), field.Children.Select(MapField).ToArray());
    }

    private AnalysisStep MapStep(MbusAnalysisStep step)
    {
        StepOutcome outcome = step.Status switch
        {
            var status when status.IsSucceeded => new SucceededStep(),
            AnalysisStepStatus.Blocked blocked => new BlockedStep(Id(blocked.cause), blocked.reason),
            AnalysisStepStatus.Failed failed => new FailedStep(AddFailure(failed.Item)),
            _ => throw new InvalidOperationException("Unknown analysis outcome.")
        };
        foreach (var notice in step.Metadata.Notices)
            switch (notice)
            {
                case Notice.Info info: AddIssue("info", "notice", info.Item); break;
                case Notice.Warning warning: AddIssue("warning", "notice", warning.Item); break;
            }
        return new(Id(step.Metadata.Id), step.Name, Span(step.Metadata.Input), step.Metadata.Dependencies.Select(Id).ToArray(), outcome);
    }

    private IReadOnlyList<string> AddFailure(DecodeFailure failure)
    {
        switch (failure)
        {
            case DecodeFailure.ParseFailed parsed:
                var error = parsed.Item;
                var id = $"diagnostic-{diagnostics.Count}";
                diagnostics.Add(new(id, "error", "parse", error.Msg, null, new(Id(error.Source.Value), error.Pos, 0)));
                return [id];
            case DecodeFailure.ValidationFailed validation:
                return FailuresModule.toList(validation.Item).Select(i => AddIssue("error", "validation", i)).ToArray();
            case DecodeFailure.EncryptionFailed protection:
                return [AddIssue("error", "protection", protection.Item)];
            default: throw new InvalidOperationException("Unknown decode failure.");
        }
    }

    private string AddIssue(string severity, string kind, Issue issue)
    {
        var id = $"diagnostic-{diagnostics.Count}";
        var fieldId = Id(issue.FieldId);
        diagnostics.Add(new(id, severity, kind, issue.Message, fieldId, spans.GetValueOrDefault(fieldId)));
        return id;
    }
}

/// <summary>Formats typed protocol values only at the presentation boundary; no JSON intermediate.</summary>
internal static class PresentationValues
{
    private static readonly FSharpOption<BindingFlags> Access = FSharpOption<BindingFlags>.Some(BindingFlags.Public | BindingFlags.NonPublic);

    private static object? SuccessfulValue<T>(Metering.Mbus.Analysis.AnalysisStep<T> step) =>
        step.Result is AnalysisStepResult<T>.Succeeded success ? success.Item : null;

    public static object? Value(MbusAnalysisStep step) => step switch
    {
        MbusAnalysisStep.DllValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.TelegramIdentification value => SuccessfulValue(value.Item),
        MbusAnalysisStep.SelectionValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.EllValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.NwlValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.AflValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.TplValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.ProtectionPreparation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.ApplicationUnprotection value => SuccessfulValue(value.Item),
        MbusAnalysisStep.AplValidation value => SuccessfulValue(value.Item),
        MbusAnalysisStep.MessageValidation value => SuccessfulValue(value.Item),
        _ => null // Parsing and blocked routing stages have no semantic value to format.
    };

    public static bool IsField(object value) => value.GetType().IsGenericType && value.GetType().GetGenericTypeDefinition() == typeof(Field<>);

    public static IEnumerable<(string Name, object? Value)> Members(object value)
    {
        var type = value.GetType();
        if (value is string || value is ReadOnlyMemory<byte> || type.IsPrimitive || type.IsEnum || value is decimal || value is FieldId || value is SourceId) yield break;
        if (value is IEnumerable sequence)
        {
            var index = 0;
            foreach (var item in sequence) yield return ($"[{index++}]", item);
        }
        else if (FSharpType.IsRecord(type, Access))
        {
            foreach (var field in FSharpType.GetRecordFields(type, Access))
                yield return (JsonNamingPolicy.CamelCase.ConvertName(field.Name), field.GetValue(value));
        }
        else if (FSharpType.IsUnion(type, Access))
        {
            var union = FSharpValue.GetUnionFields(value, type, Access);
            yield return ("case", union.Item1.Name);
            var fields = union.Item1.GetFields();
            for (var i = 0; i < fields.Length; i++) yield return (JsonNamingPolicy.CamelCase.ConvertName(fields[i].Name), union.Item2[i]);
        }
    }

    public static IEnumerable<FieldValue> Flatten(object? value, string name = "Wert", int depth = 0)
    {
        if (value is null || value is ReadOnlyMemory<byte> || depth > 10 || IsField(value)) yield break;
        var type = value.GetType();
        if (value is string || type.IsPrimitive || type.IsEnum || value is decimal || value is FieldId || value is SourceId)
        {
            var text = value switch
            {
                bool boolean => boolean ? "true" : "false",
                FieldId fieldId => FieldIdModule.value(fieldId).ToString(CultureInfo.InvariantCulture),
                SourceId sourceId => sourceId.Value.ToString(CultureInfo.InvariantCulture),
                double number => number.ToString("R", CultureInfo.InvariantCulture),
                float number => number.ToString("R", CultureInfo.InvariantCulture),
                IFormattable scalar => scalar.ToString(null, CultureInfo.InvariantCulture),
                _ => value.ToString()!
            };
            yield return new(name, text);
        }
        else
            foreach (var (member, content) in Members(value))
            {
                var childName = member.StartsWith('[') ? name + member : name == "Wert" ? member : name + "." + member;
                foreach (var item in Flatten(content, childName, depth + 1)) yield return item;
            }
    }
}
