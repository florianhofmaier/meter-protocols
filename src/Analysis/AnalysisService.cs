namespace Metering.Analysis;

public sealed class AnalysisService(IEnumerable<IProtocolAnalyzer> analyzers)
{
    private readonly IProtocolAnalyzer[] analyzers = analyzers.ToArray();
    public IReadOnlyList<ProtocolDescriptor> SupportedProtocols => analyzers.Select(a => a.Descriptor).ToArray();

    public AnalysisResponse Analyze(string input)
    {
        ArgumentNullException.ThrowIfNull(input);
        var decoded = InputInterpreter.Decode(input);
        var inputs = decoded.Select((x, i) => new InputInterpretation($"input-{i}", x.Encoding, Convert.ToHexString(x.Bytes))).ToArray();
        var matches = new List<(int Rank, AnalysisCandidate Candidate)>();
        for (var i = 0; i < decoded.Count; i++)
        {
            foreach (var analyzer in analyzers)
            {
                var match = analyzer.TryAnalyze(decoded[i].Bytes);
                if (match is null) continue;
                matches.Add((match.Rank, new($"{inputs[i].Id}:{analyzer.Descriptor.Id}", inputs[i].Id,
                    match.Confidence, match.Evidence, match.Report)));
            }
        }
        var candidates = matches.OrderByDescending(x => x.Rank).Select(x => x.Candidate).ToArray();
        var messages = new List<string>();
        if (inputs.Length == 0)
            messages.Add("Keine vollständige Byte-Darstellung erkannt. Unterstützt: Hex, 0x-Bytefolgen, \\x-Bytefolgen, JSON-Bytearrays. Log-Präfixe bitte entfernen.");
        else if (candidates.Length == 0)
            messages.Add("Die Bytes sind lesbar, passen aber zu keinem derzeit unterstützten Protokolleinstieg. Aktuell wird ein einzelnes wired M-Bus-Telegramm unterstützt; DLMS folgt später.");
        if (inputs.Length > 1)
            messages.Add("Die Eingabe ist mehrdeutig. Alle erkannten Byte-Darstellungen bleiben auswählbar; die Reihenfolge bewertet Struktur und Analyzer-Ergebnis, keine statistische Wahrscheinlichkeit.");
        if (candidates.Length > 0)
            messages.Add("Die Erkennung vergleicht nur implementierte Protokolleinstiege. Ein passender Anfang allein belegt keine gültige Nachricht.");
        return new(1, SupportedProtocols, inputs, candidates, candidates.FirstOrDefault()?.Id, messages);
    }
}
