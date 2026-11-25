using System.Text.Json.Serialization;

namespace Metering.Analysis;

public sealed record ProtocolDescriptor(string Id, string Name, string Scope);
public sealed record SourceSpan(string SourceId, int Offset, int Length);
public sealed record AnalysisSource(string Id, string Name, string Hex, bool Sensitive, SourceSpan? Origin);
public sealed record FieldValue(string Name, string Text);
public sealed record FieldDocumentation(string Description, string? Reference);
/// <summary>Protocol-provided service identity and the field that establishes it.</summary>
public sealed record MessageIdentity(string Name, string Description, string Direction, string FieldId, string Reference);

[JsonPolymorphic(TypeDiscriminatorPropertyName = "kind")]
[JsonDerivedType(typeof(ParsedField), "parsed")]
[JsonDerivedType(typeof(IncompleteField), "incomplete")]
public abstract record FieldObservation;
public sealed record ParsedField(SourceSpan Span) : FieldObservation;
public sealed record IncompleteField(string SourceId, int Offset, string Reason) : FieldObservation;

public sealed record AnalysisField(
    string Id, string Name, FieldObservation Observation,
    IReadOnlyList<FieldValue> Values, FieldDocumentation Documentation,
    IReadOnlyList<AnalysisField> Children);

public sealed record AnalysisDiagnostic(
    string Id, string Severity, string Kind, string Message, string? FieldId, SourceSpan? Span);

[JsonPolymorphic(TypeDiscriminatorPropertyName = "kind")]
[JsonDerivedType(typeof(SucceededStep), "succeeded")]
[JsonDerivedType(typeof(FailedStep), "failed")]
[JsonDerivedType(typeof(BlockedStep), "blocked")]
public abstract record StepOutcome;
public sealed record SucceededStep : StepOutcome;
public sealed record FailedStep(IReadOnlyList<string> DiagnosticIds) : StepOutcome;
public sealed record BlockedStep(string CauseId, string Reason) : StepOutcome;
public sealed record AnalysisStep(
    string Id, string Name, SourceSpan Input, IReadOnlyList<string> Dependencies, StepOutcome Outcome);

/// <summary>A UI-independent report. Field/source identifiers are local to this report.</summary>
public sealed record AnalysisReport(
    int SchemaVersion, ProtocolDescriptor Protocol, bool AllStepsSucceeded,
    IReadOnlyList<AnalysisSource> Sources, IReadOnlyList<AnalysisField> Fields,
    IReadOnlyList<AnalysisDiagnostic> Diagnostics, IReadOnlyList<AnalysisStep> Steps)
{
    /// <summary>Optional service identity, also available when subsequent analysis fails.</summary>
    public MessageIdentity? Message { get; init; }
}

public sealed record InputInterpretation(string Id, string Encoding, string Hex);
public sealed record AnalysisCandidate(
    string Id, string InputId, string Confidence, IReadOnlyList<string> Evidence, AnalysisReport Report);
public sealed record AnalysisResponse(
    int SchemaVersion, IReadOnlyList<ProtocolDescriptor> SupportedProtocols,
    IReadOnlyList<InputInterpretation> Inputs, IReadOnlyList<AnalysisCandidate> Candidates,
    string? SelectedCandidateId, IReadOnlyList<string> Messages);
public sealed record AnalysisRequest(string Input);

/// <summary>One protocol/entry-point adapter; UI and transport are outside this boundary.</summary>
public interface IProtocolAnalyzer
{
    ProtocolDescriptor Descriptor { get; }
    ProtocolMatch? TryAnalyze(ReadOnlyMemory<byte> bytes);
}

/// <summary>Rank is an ordering heuristic, never a statistical probability.</summary>
public sealed record ProtocolMatch(
    int Rank, string Confidence, IReadOnlyList<string> Evidence, AnalysisReport Report);
