using Metering.Analysis;

namespace Metering.Mbus.AnalysisAdapter;

internal static class MbusFieldDocumentation
{
    public static FieldDocumentation Describe(string name) => name switch
    {
        "Input bytes" => new("Unveränderte Bytes der ausgewählten Eingabeinterpretation. Alle Positionen beziehen sich auf die jeweils angegebene Source.", null),
        "Start Field" => new("Startkennung der vom Parser erkannten FT-1.2-Rahmenform.", "IEC 60870-5-2 §3.2, S.14; EN 13757-2 Anhang F.1/F.2, S.37"),
        "Length 1" or "Length 2" => new("Wiederholte Längenangabe des variablen Rahmens. Sie zählt die folgenden User-Data-Bytes einschließlich Kontroll- und Adressfeld.", "IEC 60870-5-2 §3.2, S.14"),
        "CRC" => new("Prüfsummenfeld des wired Rahmens. Die Berechnung und Prüfung übernimmt der vorhandene DLL-Validator.", "EN 13757-2 §5.7.2 und Anhang F.1/F.2"),
        "End Field" => new("Abschlusskennung des FT-1.2-Rahmens.", "IEC 60870-5-2 §3.2, S.14"),
        "CField" => new("Kontrollfeld der Datenverbindungsschicht; die geprüfte Interpretation steht unter den Analyzer-Werten.", "IEC 60870-5-2 §3.2; EN 13757-2 Anhang F"),
        "PrimaryAddress" => new("Adressfeld des wired Rahmens.", "IEC 60870-5-2 §3.2; EN 13757-2 Anhang F"),
        "CI-Field" or "CI-Field TPL" => new("Steuerinformation zur Struktur der folgenden Schicht. Das TPL-CI kennzeichnet außerdem das Anwendungsprotokoll und die Headerstruktur.", "EN 13757-7:2018 §5.2, Tabelle 2"),
        "TPL" => new("Transportschicht. Ihre konkrete Struktur hängt vom CI-Feld und vom Sicherheitsmodus ab.", "EN 13757-7:2018 §§5.2, 7.1"),
        "Long TPL Header" => new("Langer Transportheader mit Geräteidentifikation und weiteren Transportfeldern; zusätzliche Felder sind modusabhängig.", "EN 13757-7:2018 §7.4, Tabelle 12"),
        "Short TPL Header" => new("Kurzer Transportheader; zusätzliche Felder sind modusabhängig.", "EN 13757-7:2018 §7.3, Tabelle 11"),
        "APL" or "APL Data" => new("Anwendungsdaten. Beim M-Bus-Anwendungsprotokoll enthalten sie Records und gegebenenfalls herstellerspezifische Daten.", "EN 13757-3:2025 §§6.1–6.2, Tabellen 1–2"),
        "Data Record" => new("Ein Datensatz verbindet Dateninformation, Wertinformation und Datenwert. DIB und VIB beschreiben Kodierung, Einheit und Multiplikator.", "EN 13757-3:2025 §6.2, Tabelle 2"),
        "DIB" => new("Dateninformationsblock: DIF und gegebenenfalls DIFE beschreiben unter anderem Länge, Kodierung und Funktion der Daten.", "EN 13757-3:2025 §§6.3.1–6.3.3, Tabellen 3–4"),
        "VIB" => new("Wertinformationsblock: VIF und gegebenenfalls VIFE bestimmen unter anderem Einheit und Multiplikator.", "EN 13757-3:2025 §§6.4.1–6.4.2, Tabellen 9–10"),
        "Data" => new("Datenwert des Records. Die Interpretation benötigt die zugehörigen Daten- und Wertinformationsblöcke; die Bytefolge allein ist kein physikalischer Messwert.", "EN 13757-3:2025 §§6.1–6.4"),
        "32 Bit Integer" => new("Vier Bytes des Datenwerts. Die zugeordnete geprüfte Interpretation stammt aus der erfolgreichen Record-Validierung des Analyzers.", "EN 13757-3:2025 §6.3.3, Tabelle 4"),
        _ when name.StartsWith("Format FT 1.2", StringComparison.Ordinal) => new("Vom wired DLL-Parser erkannter FT-1.2-Rahmen. Strukturelles Lesen und semantische Validierung sind getrennte Analyseschritte.", "IEC 60870-5-2 §3.2, S.14; EN 13757-2 §5.7.1"),
        _ => new("Vom Analyzer beobachtetes Feld. Byteposition, geprüfte Werte und zugehörige Diagnosen werden getrennt ausgewiesen. Für dieses Feld ist noch keine eigene Erläuterung hinterlegt.", null)
    };
}
