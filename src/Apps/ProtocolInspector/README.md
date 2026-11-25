# Protocol Inspector

Lokale React-/TypeScript-Webanwendung für die Bibliotheken in diesem Repository.
Derzeit: ein einzelnes wired M-Bus-Telegramm. DLMS/COSEM ist ein geplanter weiterer
Adapter, noch kein unterstütztes Analyseformat.

Die Anwendung im Browser über **HTTP** öffnen, nicht durch Doppelklick auf
`Client/index.html`. Diese HTML-Datei ist der Vite-Quelleneinstieg; React und
TypeScript benötigen den Dev-Server oder den gebauten Client hinter dem .NET-Host.
Die normale Adresse des gemeinsamen Hosts ist `http://127.0.0.1:5080/`.

## Voraussetzungen

- .NET SDK 10 (wie die vorhandenen Analyzer-Projekte)
- Node.js 22.12+ oder 24 und pnpm 11

## Entwicklung

Vom Repository-Root den Server starten:

```sh
dotnet run --project src/Apps/ProtocolInspector/Server --urls http://127.0.0.1:5080
```

In einem zweiten Terminal:

```sh
cd src/Apps/ProtocolInspector/Client
pnpm install --frozen-lockfile
pnpm dev
```

Die von Vite ausgegebene URL öffnen (normalerweise `http://127.0.0.1:5173`).
Vite leitet `/api` an den lokalen .NET-Server auf Port 5080 weiter.

## Ein gemeinsamer Host

```sh
cd src/Apps/ProtocolInspector/Client
pnpm install --frozen-lockfile
pnpm build
```

Danach den Server wie oben starten und `http://127.0.0.1:5080` öffnen. Er liefert
den Client aus `../Client/dist` mit aus. Zum Weitergeben werden die Serverdateien
und `Client/dist` mit derselben Verzeichnisbeziehung benötigt; `dotnet publish`
allein baut und verpackt den Client nicht automatisch.

## Bedienung und Grenzen

- Hex mit/ohne Leerzeichen, `0x`-Bytes, `\x`-Bytes und dezimale JSON-Bytearrays
  sind möglich. Optional kann Hex mit `hex:` gekennzeichnet werden.
- Die Byteansicht zeigt Hexwerte mit Offsets und gekoppelter Feldauswahl.
- Unter **Erkennung** erklärt das Ranking die gewählte Protokollinterpretation.
  Es vergleicht Parser-/Validator-Ergebnisse der registrierten Adapter; es ist keine
  statistische Wahrscheinlichkeit und keine Erkennung aller existierenden Protokolle.
- Ungültige Prüfsummen, abgeschnittene Eingaben und nicht implementierte Inhalte
  bleiben mit den vom Analyzer erreichbaren Teilergebnissen untersuchbar.
- Unbekannte Bytes werden angezeigt, ohne sie als M-Bus auszugeben. Log-Zeitstempel
  und beliebige Präfixe werden nicht automatisch entfernt. Mehrere Frames, rohe
  APL-/TPL-Ausschnitte und Wireless-Frames besitzen noch keinen eigenen Einstieg.
- Kein externer Schlüssel wird angenommen. Verschlüsselte Inhalte bleiben bei
  fehlendem Schlüssel als Bytes mit Protection-Diagnosen erhalten.
- Feldauswahl und Byteauswahl laufen im Browser. Fehlgeschlagene Felder haben einen
  bekannten Start, keinen erfundenen vollständigen Bytebereich.
- JSON speichert alle Kandidaten samt Bericht und lässt sich über **Datei öffnen**
  in einer bereits geladenen Oberfläche auch bei nicht verfügbarem Analysedienst öffnen. HTML exportiert die ausgewählte
  Interpretation als eigenständigen lesbaren und druckbaren Bericht.
- **Alle Analyseschritte erfolgreich** ist keine vollständige Normkonformitätsaussage.
  Erläuterungen und Normstellen sind zunächst für zentrale Frame-/Record-Felder
  hinterlegt; weitere Felder zeigen einen ausdrücklichen Hinweis auf fehlende Metadaten.
- Eingaben und Berichte werden serverseitig nicht gespeichert. Text höchstens
  393216 Zeichen, dekodierte Eingabe höchstens 65536 Bytes; lokale Berichtsdateien
  höchstens 2 MiB. Die Oberfläche benötigt keine extern geladenen Assets.

## Erweiterung / austauschbare UI

- `src/Analysis`: UI-/HTTP-unabhängiger Vertrag, Eingabenormalisierung, Kandidatenwahl.
- `src/Mbus/AnalysisAdapter`: Adapter auf `MbusAnalyzer`; keine zweite Protokollprüfung.
- `Server`: Registrierung der Adapter und Transport über HTTP.
- `Client`: reine Darstellung und lokaler Bedienzustand.

Ein weiterer Protokolladapter implementiert `IProtocolAnalyzer` und wird im Host
registriert. Der Vertrag enthält keine M-Bus-Layer-Union. Eine Desktop-UI kann
`AnalysisService` im selben Prozess verwenden oder dieselbe HTTP-Schnittstelle
ansprechen; sie benötigt keine React-Komponenten oder Web-ViewModels.

`POST /api/analyze` nimmt `{ "input": "10 5B 01 5C 16" }` entgegen und liefert
Schema-Version 1. `GET /api/protocols` listet nur implementierte Adapter,
`GET /api/health` prüft die Erreichbarkeit des Hosts. `GET /api/schema` liefert
das aus den .NET-Vertragstypen erzeugte JSON-Schema. Die Anwendung verwendet
Schema-Version 1; inkompatible Änderungen benötigen eine neue Version. Optionale
Erweiterungen wie `report.message` bleiben kompatibel mit älteren Berichten.

Die Zusammenfassung zeigt den im Protokollkern erkannten Link-Dienst (ACK, SND-NKE,
REQ-UD1, REQ-UD2, SND-UD, SND-UD2 oder RSP-UD), Richtung und das typbestimmende Feld.
Diese Erkennung bleibt auch bei späteren Fehlern verfügbar. Sie bedeutet nicht,
dass die vollständige Nachrichtenabbildung oder Nutzlast unterstützt ist.

## Tests

```sh
dotnet test src/Analysis.Tests/Analysis.Tests.csproj
dotnet test src/Mbus/Analyzer.Tests/Analyzer.Tests.fsproj
```

Im Client-Verzeichnis:

```sh
pnpm test
pnpm build
```
