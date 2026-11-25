# Analyse der Codeformatierung

Stand: 2026-11-01, vor der Einführung der gemeinsamen Formatter-Konfiguration.

Untersucht wurde der vorhandene Quellcode einschließlich neuer, noch nicht
eingecheckter Dateien. Ausgeschlossen wurden `.git`, `bin`, `obj`, `node_modules`
und `dist`. Die Analyse betrifft Formatierung, keine Protokollsemantik oder
Architekturprüfung. Gelöschte Dateien wurden nicht berücksichtigt.

## Umfang und messbare Auffälligkeiten

| Sprache | Dateien | Zeilen | Zeilen > 120 Zeichen | Zeilen > 160 Zeichen | Dateien ohne abschließenden Zeilenumbruch | Dateien mit UTF-8-BOM |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| F# | 238 | 23 553 | 257 | 38 | 55 | 6 |
| C# | 17 | 1 594 | 50 | 22 | 4 | 3 |
| TypeScript | 4 | 167 | 23 | 11 | 0 | 0 |
| TSX | 2 | 202 | 41 | 32 | 0 | 0 |
| CSS | 1 | 1 519 | 1 | 0 | 0 | 0 |

Alle untersuchten Dateien verwenden LF-Zeilenenden. Tabs und nachgestellte
Leerzeichen wurden nicht gefunden. Zeilenlängen wurden einschließlich
Kommentaren und Zeichenketten gezählt; eine Überschreitung ist deshalb nicht
automatisch ein Formatierungsfehler. Einrückungen abseits eines Vielfachen von
vier Leerzeichen kommen in F# vor, können aber auch legitime Ausrichtungen sein.
Im Frontend sind zwei Leerzeichen bereits üblich.

## Wiederkehrende Muster

- **Abstände bei F#-Typannotationen wechseln.** Beispiele sind
  `let parseU8 : Parser<uint8>` und `let parseU24LittleEndian: Parser<uint32>`
  in `src/Common/Parsers/Binary.fs`. Auch zwischen Parametern und Typannotationen
  kommen beide Schreibweisen vor.
- **F#-Records und Listen haben unterschiedliche Klammerpositionen.**
  `src/Common/Validators/Core.fs` verwendet etwa für `Issue` eine öffnende
  Record-Klammer auf einer eigenen Zeile. In anderen Dateien stehen Klammern
  und Felder kompakter zusammen. Die gewählte Fantomas-Konfiguration soll
  diese Entscheidung übernehmen.
- **Kurze F#-Ausdrücke sind häufig über mehrere Zeilen verteilt.**
  Beispielsweise stehen in `src/Common/Utility/Result.fs` selbst einfache
  Member-Rückgaben auf einer Folgezeile. Auch Parametersignaturen und
  Pattern-Match-Ergebnisse sind teilweise stark auseinandergezogen.
  Fantomas kann solche Stellen zusammenziehen.
- **Lange F#-Testdaten und Patterns fallen auf.** Beispiele stehen in
  `src/Mbus/Protocol.Tests/BcdWritersTests.fs`, `EncodingTests.fs` und
  `LayerCodecTests.fs`. Automatische Umbrüche sollten über den Formatter
  erfolgen, nicht über manuelle Änderungen an Testdaten.
- **C# enthält lange Aufrufketten und Ausdrücke.** Beispiele stehen in
  `src/Analysis.Tests/AnalysisTests.cs` und `src/Analysis/AnalysisService.cs`.
  Die Wahl eines C#-Formatters allein garantiert keine harte Zeilenbegrenzung.
- **Das Frontend ist stellenweise sehr dicht geschrieben.**
  `src/Apps/ProtocolInspector/Client/src/contracts.ts` enthält lange
  Typdefinitionen auf einzelnen Zeilen; `src/App.tsx` enthält lange JSX-Zeilen
  und mehrere Anweisungen auf einer Zeile. CSS verwendet häufig kompakte
  Deklarationen wie `font-synthesis:none`.

## Gewählte Regeln

Nach Rücksprache festgelegt:

1. Zielbreite: 100 Zeichen.
2. Mehrzeilige F#-Klammern: Stroustrup.
3. Umfang: F#, C# und Frontend.

Die verbindliche Konfiguration liegt in `.editorconfig`, dem lokalen
.NET-Tool-Manifest und der Prettier-Konfiguration des Clients. Die gemeinsamen
Regeln und Befehle stehen in [CONTRIBUTING.md](../CONTRIBUTING.md).

## Formatter-Probelauf

Fantomas 8.0.10 wurde als lokales Tool mit fester Version wiederhergestellt.
Ein schreibgeschützter Probelauf mit den Standardeinstellungen meldet:

- 228 F#-Dateien benötigen Formatierung.
- 9 F#-Dateien entsprechen bereits den Standardeinstellungen.
- 1 F#-Datei kann nicht eingelesen werden:
  `src/Dlms/Protocol/Xdlms/XdlmsApduDecodeSteps.fs`, Zeile 51,
  `FS0010: Unexpected symbol '|' in pattern matching`.

Der Probelauf hat keine Quelldateien geändert. Die betreffende Datei enthält
einen leeren Match-Zweig und ist gezielt in `.fantomasignore` ausgenommen, bis
die fehlende Implementierung ergänzt wird. Sie blieb bytegenau unverändert.

## Umstellung

- 237 verarbeitbare F#-Dateien wurden mit dem vereinbarten Stil geprüft und
  formatiert; einschließlich der UTF-8-Vereinheitlichung änderten sich 230 Dateien.
- Alle 17 C#-Dateien wurden geprüft; 6 erhielten Formatierungsänderungen.
- Das Frontend wurde mit Prettier formatiert. Die Abhängigkeit ist auf Version
  3.9.10 festgelegt und die pnpm-Sperrdatei aktualisiert.
- Der gemeinsame Prüfaufruf `bash scripts/format.sh --check` ist erfolgreich.
  Ein zweiter Formatter-Lauf verlangt keine weiteren Änderungen.
- Eine GitHub-Actions-Pipeline verwendet denselben Prüfaufruf.

100 Zeichen sind eine Zielbreite, keine harte Grenze für alle Zeichenketten oder
Kommentare. Insbesondere erzwingt der C#-Whitespace-Formatter keine maximale
Zeilenlänge. Die Ausnahme für den unvollständigen DLMS-Code bleibt ausdrücklich
sichtbar und ist kein Nachweis für dessen Kompilierbarkeit.

## Validierung

- Gemeinsame Formatierungsprüfung: erfolgreich, einschließlich F#, C# und Frontend.
- Frontend: Produktionsbuild und alle 5 vorhandenen Tests erfolgreich.
- Protocol-Inspector-Server: Build erfolgreich.
- Prettier-AST-Prüfung (`--debug-check`) auf den gesicherten ursprünglichen
  TypeScript-, TSX-, CSS- und HTML-Dateien mit der gewählten Konfiguration:
  erfolgreich.
- .NET: alle 14 vorhandenen Testprojekte einzeln geprüft; 504 Tests in
  12 Projekten erfolgreich, zwei Projekte konnten nicht ausgeführt werden.
- `Common/Security/Cryptography.Tests`: vorhandene Tests passen nicht mehr zu
  den Kryptografie-APIs (`FS0039`, unter anderem `AesCbc` und frühere Fehlerfälle).
  Eine temporäre Kopie mit den gesicherten ursprünglichen Testdateien zeigt
  dieselben Fehler gegen die Bibliothek.
- `Mbus/MbusDevices.Tests`: das Testprojekt verwendet `net9.0`, seine
  Projektabhängigkeit `MbusDevices` dagegen `net10.0` (`NU1201`). Diese
  Projektdateien wurden durch die Formatierungsumstellung nicht verändert.
- DLMS-Protokollprojekt: Build bestätigt den bereits vor der Formatierung
  erkannten Syntaxfehler `FS0010` in der unveränderten, ausgenommenen Datei
  `XdlmsApduDecodeSteps.fs`, Zeile 51.

Die fehlgeschlagenen Projekte wurden nicht durch fachliche Änderungen innerhalb
dieser Formatierungsumstellung repariert. Ein vollständig grüner Build des
gesamten Repositorys ist damit nicht nachgewiesen.

Die Ergebnisse beschreiben den Ausgangszustand und sind keine Liste verbleibender
Fehler nach einer späteren Formatierung. Bereits vorhandene Arbeitsänderungen
müssen erhalten bleiben; ein Formatierungs-Commit sollte getrennt von fachlichen
Änderungen entstehen.
