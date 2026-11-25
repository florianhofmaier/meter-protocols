# Gemeinsamer Code-Standard

## Einrichtung

Benötigt werden das in `global.json` festgelegte .NET-SDK (10.0.201 oder ein
neuerer Patch derselben Feature-Band), Node.js 24 und pnpm 11.25.0.
Einige bestehende Testprojekte benötigen zusätzlich die .NET-9-Runtime.

Vom Repository-Root:

```sh
dotnet tool restore
pnpm --dir src/Apps/ProtocolInspector/Client install --frozen-lockfile
```

## Automatische Formatierung

```sh
# Alle unterstützten Quellsprachen formatieren
bash scripts/format.sh --write

# Nur prüfen, ohne Dateien zu verändern
bash scripts/format.sh --check
```

Ohne Argument prüft das Skript ebenfalls nur. GitHub Actions führt dieselbe
Prüfung bei Pushes und Pull Requests aus. Die Pipeline ändert keine Dateien.
Erzeugte Dateien in `bin`, `obj`, `node_modules` und `dist` sind ausgeschlossen.

| Sprache | Werkzeug | Gemeinsame Regeln |
| --- | --- | --- |
| F# (`.fs`, `.fsi`, `.fsx`) | Fantomas 8.0.10 als lokales .NET-Tool | 100 Zeichen, vier Leerzeichen, Stroustrup-Klammern |
| C# | `dotnet format whitespace` aus dem ausgewählten SDK | Vier Leerzeichen, öffnende Blockklammer auf eigener Zeile |
| Frontend | Prettier 3.9.10 als lokale Entwicklungsabhängigkeit | 100 Zeichen, zwei Leerzeichen, einfache Anführungszeichen |

`.editorconfig` und die eingecheckte Prettier-Konfiguration sind maßgeblich.
100 Zeichen sind die Zielbreite. Formatter dürfen etwa lange Zeichenketten oder
Kommentare stehen lassen; `dotnet format whitespace` erzwingt keine maximale
C#-Zeilenlänge. Lange C#-Ausdrücke bei inhaltlichen Änderungen lesbar umbrechen.
Dateien verwenden UTF-8, LF und einen abschließenden Zeilenumbruch.

Im Editor Formatieren beim Speichern aktivieren und für F# die lokale
Fantomas-Version bzw. für das Frontend die lokale Prettier-Version auswählen.
Die Prüfung oben entscheidet unabhängig von individuellen Editor-Einstellungen.
Formatter-Versionen bewusst aktualisieren und deren Änderungen separat prüfen.

Formatierter Code muss weiterhin lesbar sein. Tief verschachtelte Record-Patterns,
die Fantomas weit nach rechts ausrichtet, in überschaubare Match-Schritte zerlegen.
Eine gemeinsam verwendete fachliche Erkennungsregel darf dafür lokal gebündelt
werden. Die Ausgabe des Formatters bei solchen Änderungen auch visuell prüfen;
eine erfolgreiche Formatierungsprüfung allein genügt nicht.

### Bestehende Ausnahme

`src/Dlms/Protocol/Xdlms/XdlmsApduDecodeSteps.fs` enthält bereits einen
unvollständigen Match-Zweig (FS0010). Die Datei ist in `.fantomasignore` gezielt
ausgeschlossen. Sobald die fehlende Implementierung ergänzt ist, den Eintrag
entfernen und die Datei formatieren. Die Ausnahme ist keine erfolgreiche
Formatierungs- oder Buildprüfung dieser Datei.

## Codegestaltung

Die folgenden Regeln ergänzen die automatische Formatierung. Die Bezeichner in
Normen und öffentlichen APIs bleiben maßgeblich. Bestehende öffentliche Namen
werden bei einer reinen Formatierung nicht umbenannt.

### Protocol Modeling

When modeling protocol states, prefer types that make invalid states unrepresentable. Use discriminated unions, single-case wrappers, and specific records where they express protocol alternatives or required data better than broad records with optional members.

Only allow invalid combinations in the model when excluding them would add disproportionate complexity for the current scope. If taking that shortcut, make the tradeoff explicit in the response and keep validation close to the boundary where the invalid state can enter.

### Project Conventions

- Organize M-Bus messages as one companion module per message and one file per module. Keep each message type and its operations together in the equally named file. Shared address types follow the same pattern; avoid collective `MessageTypes.fs` or `MessageWriters.fs` files.
- Use field names from the applicable standard. Do not invent named fields for parser bookkeeping or unread byte ranges.
- Document public functions and members in the Common libraries with XML documentation comments.
- Name pure validation functions returning `Validation<_>` `validate`. Functions that need parser execution, decryption, source registration, or other decoder context should be named `decode`.
- Keep security context types limited to information supplied from outside the frame, such as keys and key selection material. Do not put protocol fields from the current frame, such as meter addresses, access numbers, configuration fields, or C/TPL/APL fields, into external security context types.
- If a security operation needs protocol fields that are not available in the current frame model, fail explicitly or extend the frame model. Do not ask callers to provide frame-derived protocol fields through the external security context.
- Do not introduce wrapper modules or helper functions for trivial representation conversions unless they own a real protocol decision or remove meaningful duplication.

Für Änderungen an M-Bus-Semantik gelten zusätzlich die lokalen Normen und die
Nachweispflichten unter [M-Bus Standards in AGENTS.md](AGENTS.md#mbus-standards).
Die Unterscheidung von Anforderungen, akzeptierter Architektur, offenen Entwürfen
und verifizierter Konformität ist dort ebenfalls beschrieben.

## Änderungen prüfen

Nach einer Formatierungsumstellung die Prüfung oben sowie Builds und vorhandene
Tests der betroffenen Projekte ausführen. Für inhaltliche Änderungen die passenden
Tests ergänzen oder anpassen; Formatierungsregeln benötigen keine eigenen
Unit-Tests. Keine fachlichen Änderungen in Formatierungs-Commits mischen und
bereits vorhandene Arbeitsänderungen erhalten.

Die [Formatierungsanalyse](doc/code-formatting-analysis.md) beschreibt den
Ausgangszustand und die Ergebnisse der ersten Umstellung.
