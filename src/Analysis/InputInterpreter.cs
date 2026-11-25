using System.Text.Json;
using System.Text.RegularExpressions;

namespace Metering.Analysis;

public sealed record DecodedInput(string Encoding, byte[] Bytes);

/// <summary>Recognizes complete textual byte representations without discarding arbitrary text.</summary>
public static partial class InputInterpreter
{
    public const int MaxBytes = 65536;
    public const int MaxTextLength = MaxBytes * 6;

    public static IReadOnlyList<DecodedInput> Decode(string input)
    {
        if (input.Length > MaxTextLength)
            throw new ArgumentException($"Die Eingabe ist zu groß (maximal {MaxTextLength} Zeichen).");
        var text = input.Trim();
        if (text.Length == 0) return [];
        var results = new List<DecodedInput>();
        void Add(string name, byte[] bytes)
        {
            if (bytes.Length == 0 || bytes.Length > MaxBytes) return;
            if (!results.Any(r => r.Bytes.AsSpan().SequenceEqual(bytes))) results.Add(new(name, bytes));
        }

        if (text.StartsWith('['))
        {
            try
            {
                using var doc = JsonDocument.Parse(text);
                if (doc.RootElement.ValueKind == JsonValueKind.Array && doc.RootElement.GetArrayLength() <= MaxBytes)
                    Add("JSON-Bytearray (dezimal)", doc.RootElement.EnumerateArray().Select(x => x.GetByte()).ToArray());
            }
            catch (Exception e) when (e is JsonException or InvalidOperationException or FormatException) { }
        }

        var payload = text.StartsWith("hex:", StringComparison.OrdinalIgnoreCase) ? text[4..].Trim() : text;
        if (CompactHex().IsMatch(payload))
        {
            var compact = payload.StartsWith("0x", StringComparison.OrdinalIgnoreCase) ? payload[2..] : payload;
            if (compact.Length % 2 == 0) Add("Hex", Convert.FromHexString(compact));
        }
        else if (HexTokens().IsMatch(payload))
        {
            var tokens = ByteToken().Matches(payload).Select(m => m.Groups[1].Value);
            Add("Hex (Bytefolge)", Convert.FromHexString(string.Concat(tokens)));
        }
        return results;
    }

    [GeneratedRegex(@"^(?:0[xX])?[0-9a-fA-F]+$")]
    private static partial Regex CompactHex();
    [GeneratedRegex(@"^\s*(?:(?:0[xX]|\\x)?[0-9a-fA-F]{2})(?:[\s,:;\-]+(?:0[xX]|\\x)?[0-9a-fA-F]{2}|(?=\\x)\\x[0-9a-fA-F]{2})*\s*$")]
    private static partial Regex HexTokens();
    [GeneratedRegex(@"(?:0[xX]|\\x)?([0-9a-fA-F]{2})")]
    private static partial Regex ByteToken();
}
