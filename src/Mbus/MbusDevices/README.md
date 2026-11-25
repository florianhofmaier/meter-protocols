# M-Bus-Gerät für C#

Die Fassade verwendet die aktuelle F#-Gerätelaufzeit und `MbusCodec`. Ein Aufrufer
benötigt keine F#-Protokolltypen:

```csharp
using Metering.Mbus;

var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);
device.UpdateUserData(data =>
{
    data.AddInt32(1234, record => record.AsCubicMetersExpMinus3()
        .WithStorageNumber(0)
        .WithTariff(0)
        .WithSubUnit(0));
});

await device.RunAsync(stream, primaryAddress: 5, cancellationToken);
```

`stream` ist ein vom Aufrufer bereitgestellter Duplex-Stream, etwa eine serielle
Verbindung. Das Gerät schließt ihn nicht. Eingabeende beendet den Lauf; ein Abbruch
über das Token führt zu `OperationCanceledException`.

`IMbusUserDataBuilder` beschreibt nur `byte[] Build()`. Eigene Implementierungen
liefern den vollständigen codierten RSP-UD-Anwendungsdatenbestand; die Fluent-Methoden
gehören zum mitgelieferten konkreten `MbusUserDataBuilder`.

Der normale Einstieg ist `UpdateUserData(Action<MbusUserDataBuilder>)`. Jeder Aufruf
verwendet einen neuen Fluent-Builder und ersetzt den vollständigen Datenbestand nach
erfolgreicher Verarbeitung. Ein leerer Callback löscht den Inhalt.

Für eigene Implementierungen gibt es `UpdateUserData(IMbusUserDataBuilder)`:

```csharp
IMbusUserDataBuilder customBuilder = new MyUserDataBuilder(values);
device.UpdateUserData(customBuilder);
```

Das Device ruft `Build()` genau einmal während dieses Updates auf und behält die
Builder-Instanz nicht. Beide Wege kopieren die erzeugten Bytes, parsen und validieren
sie mit den vorhandenen Protokolltypen und prüfen, ob sie in den unterstützten
Antwortformaten ausgegeben werden können. Erst danach wird der Bestand atomar
ersetzt. Änderungen an Buildern oder Eingabepuffern nach dem Aufruf beeinflussen
bereits übernommene Daten nicht.

Fehler werden synchron am Update-Aufruf gemeldet. Ausnahmen aus dem Callback oder
`Build()` werden weitergereicht; ungültige, unvollständige oder nicht ausgebbare
Daten führen zu `ArgumentException`. Der bisherige Datenbestand bleibt aktiv,
eine laufende Auslesung und `RunAsync` laufen weiter. Die nächste erfolgreiche
Aktualisierung kann denselben eigenen Builder erneut verwenden.

Eigene Builder liefern APL-Daten ohne CI, TPL-Header oder Telegrammrahmen. Leere
Bytes löschen den Inhalt; `null` ist ungültig. Ein abschließender Herstellerblock
mit `0Fh` bleibt erhalten. `1Fh` wird als Eingabe abgewiesen, da der Builder den
vollständigen Bestand liefert und das Device die Telegrammfortsetzung bestimmt.
Die Summe der Records darf länger als ein Telegramm sein.

Die konfigurierte Primäradresse liegt bei 0 (unkonfiguriert) oder 1–250. Die
Identität besteht aus einer Nummer von 1–99999999, drei Hersteller-Großbuchstaben,
einer Version von 0–254 und einem `MbusDeviceType`. Die Eingabe wird vor der
Konvertierung in Bytefelder geprüft.

Die Fassade verwendet `EncodingOptions.Plain`. Die F#-API bietet zusätzlich
Mode-5-Konfiguration. Der C#-Builder unterstützt die vom Codec implementierten
skalaren Darstellungen und VIF-Tabellen sowie die vorhandenen DIB-Optionen.

Die ausführbaren Beispiele und Duplexprüfungen stehen in
[den Gerätetests](../MbusDevices.Tests/MbusDeviceTests.cs).

## Prinzip und Umfang des Fluent-Builders

Der Builder bildet normale Antwortrecords in drei Schritten ab:

1. `AddInt32`, `AddBcd8Digit` usw. wählen die Darstellung des Rohwerts und damit
   das Datenfeld des DIF.
2. `As…` wählt Wertbedeutung, Einheit und Multiplikator im VIB.
3. `WithFunctionField`, `WithStorageNumber`, `WithTariff` und `WithSubUnit` setzen
   die übrigen DIB-Felder. `WithRecordError` ergänzt einen Fehler-VIFE.

Das entspricht EN 13757-3:2025, §6.2/Tabelle 2. Der Builder sammelt unveränderliche
`DataRecordRsp`-Werte; die vorhandenen F#-Writer und `Codec.UserData.encode`
übernehmen die Codierung. Es gibt keine zweite C#-Tabelle für DIF/VIF-Codes.

Unterstützte Add-Methoden sind `AddInt8/16/24/32/48/64`, `AddReal32`,
`AddBcd2Digit/4Digit/6Digit/8Digit/12Digit`, `AddPositiveBcd`, `AddNegativeBcd`,
`AddText`, `AddBinary` und `AddNoData`. Die Darstellungsgrenzen stammen aus
§6.3.3, Tabellen 4/5, sowie Anhang A. Feste BCD-Darstellungen nehmen hier nur
nichtnegative Dezimalziffern an; negative variable BCD-Werte haben ein eigenes LVAR.
Textdaten verwenden ISO-8859-1 mit höchstens 191 Zeichen. Binärdaten werden in
Drahtreihenfolge übergeben und kopiert; ihre darstellbaren Längen sind 0–15, 16,
20, 24, 28, 32, 48 und 64 Bytes.

`As(valueType, unit, scaler)` erschließt die im Codec hinterlegten primären und
erweiterten VIF-Tabellen (§6.4, Tabellen 10–15). Nicht hinterlegte Kombinationen
werden abgewiesen. Die bisherigen benannten Volumenmethoden und
`AsFabricationNumber` bleiben als bequeme Einstiege verfügbar.
`AsTextUnit` erlaubt eine Klartexteinheit ohne Erweiterungen mit höchstens
255 ASCII-Zeichen (§6.4.1/Tabelle 11 und Anhang C.2).

```csharp
var data = new MbusUserDataBuilder()
    .AddInt64(123456, record => record.As(
        MbusValueType.Energy, MbusUnit.WattHours, 1000m))
    .AddReal32(21.5f, record => record.As(
        MbusValueType.FlowTemperature, MbusUnit.Celsius))
    .AddBcd8Digit(12345678, record => record.AsFabricationNumber())
    .AddNoData(record => record.AsCubicMetersExpMinus3()
        .WithRecordError(MbusRecordError.NoDataAvailable));
```

Es werden **Rohwerte** übergeben: `123456` mit Multiplikator `1000` bedeutet
123456000 Wh. Der Builder skaliert oder rundet die Werte nicht. VIF-spezifische
Interpretationen, etwa unsigned Zahlen oder Statusbits statt signed Zahlen,
bleiben bei diesem allgemeinen Einstieg Aufgabe des Aufrufers. Auch reservierte
Bitmuster für ungültige Werte sind von normalen Zahlen zu unterscheiden
(Anhang A, Tabellen A.2/A.3). Die Prüfung sichert Darstellbarkeit und vorhandene
VIF-Codes; sie ist keine vollständige semantische VIF/DIF- und Inhaltsprüfung.

Fehlgeschlagene Add-Aufrufe verändern den bisherigen Inhalt nicht. Das Callback
muss den ihm übergebenen Record-Builder verwenden; zwischengespeicherte Records
aus anderen Aufrufen werden abgewiesen. Jeder Record wird vor der Übernahme
durch den Codec codiert. `WithRecordError` kann etwa
„No data available“ ausdrücken (§6.4.8/Tabelle 19). Das unterscheidet sich vom
Function Field `ValueInErrorState`, das einen Wert während eines Fehlerzustands
kennzeichnet (§6.3.4/Tabelle 7).

### Grenzen des bisherigen Konzepts

Die folgenden Fälle benötigen mehr als zusätzliche benannte Einheitenmethoden.
Die Normverweise beziehen sich auf die lokale EN 13757-3:2025; Seitenzahlen sind
die gedruckten Normseiten.

| Fall | Grenze und erforderliche Erweiterung | Normstelle |
| --- | --- | --- |
| Datum, Zeit, Sommerzeit, Listening Window | VIF und Datendarstellung sind gekoppelt. Eigene Typen müssen auch Invalid-Bits, Wildcards und Zusatzflags tragen können. Der allgemeine `As`-Einstieg weist diese zusammengesetzten Typen ab. | Tabelle 10; Tabelle 12; Anhang A, Tabellen A.5–A.12, S.49–55 |
| Protokollcontainer und Zulassungsklassen | Spezifische DIF/LVAR-Kombinationen; `AddBinary` mit seinem normalen binären LVAR ist kein passender Container. Der allgemeine Einstieg weist diese VIFs ab. | Tabelle 12, Fußnote f, S.21; §6.5.4, S.38 |
| Eigene Einheit mit Skalierung oder weiteren VIFEs, etwa iGal/h | `VibRsp.Text` enthält nur einen String. Das Modell braucht zusätzlich Extensions und Fehlercodes; der Parser weist `FC` derzeit ab. `AsTextUnit` unterstützt nur `7C`. | Anhang C.2, Tabelle C.3, S.57–58 |
| Historischer Wert mit OBIS-Registernummer | Die abschließende DIFE `00` unterscheidet Register- von Speichernummern. `Dib` hält diese Information nicht; beim erneuten Codieren geht sie verloren. | §6.3.5/6.3.7, S.15; §F.2.6, S.70 |
| Negative feste BCD-Werte und BCD-Fehlersignale | Die festen `Value`-Varianten und Converter akzeptieren nur Ziffern 0–9. Führendes F für Minus und A–E für ungültige Werte brauchen ein erweitertes Modell. | Anhang A, Tabelle A.1, S.48; §B.2.1, S.56 |
| Vollständiger Speichernummernbereich | Die Norm sieht 41 Bit vor; der vorhandene Konstruktor und die C#-Methode akzeptieren nur `int`, obwohl der interne Wert bereits `uint64` ist. | §6.3.7, Tabelle 8, S.15 |
| Kompakte Profile | Basiszeit, Basiswert, Intervalle und Folgeelemente sind voneinander abhängig. Dafür ist ein Profil-Builder für mehrere gekoppelte Records sinnvoll. | Anhang F.2, S.67–71 |
| Abschließender Herstellerblock | Ist kein normaler DIB/VIB/Wert-Record. `RspUdData` unterstützt ihn bereits, die Fluent-Sammlung enthält ausschließlich `DataRecordRsp`. Eine eigene Tail-Operation fehlt. Eigene `IMbusUserDataBuilder` können `0F` bereits liefern. | §6.1/Tabelle 1; §6.6, S.38–39 |

Weitere normale VIFEs, etwa Flussrichtung, Phase oder Basisbedingungen, fehlen
überwiegend nur in der C#-API; das Protokollmodell hat dafür bereits Erweiterungen
(§6.4.6, Tabellen 16/17). Ein eigener Byte-Builder umgeht dagegen keine Lücken,
bei denen die anschließende Device-Validierung Information verwirft oder ablehnt.

Befehle und Readout Selection brauchen einen separaten Command-/Selection-Builder:
VIFEs haben dort andere Bedeutungen als in Antworten (§6.4.7/Tabelle 18).
Mehrere Anwendungen benötigen zusätzlich mehrere Datenbestände im Device (§7).
Compact/Format Frames nach Anhang G sind ein eigenes zustandsabhängiges Format
für Wireless M-Bus; sie sind nicht mit kompakten Profilen nach Anhang F gleichzusetzen.

## Mehrtelegramme und Link-Zustand

Eine neue Auslesung übernimmt einen festen Datenbestand. Updates während einer
laufenden Folge werden erst beim Start einer neuen Auslesung sichtbar. Gleiches
FCB wiederholt den zuletzt gelieferten Block; gewechseltes FCB fordert den nächsten
an. Nach dem letzten Block beginnt die nächste neue Anfrage wieder bei Block 0.
Die Access Number wird auch bei Wiederholungen fortgeschaltet. Primäradresse und
Sekundäradressweg 253 haben getrennte Zustände; Diagnoseadresse 254 verwendet den
Primäradresszustand.

`Devices/Readout` bestimmt das Nutzbudget aus den Protokollgrenzen und teilt nur an
Record-Grenzen. Mehrteilige Antworten enthalten die Blockkennung mit VIF
`FD FD 00`. Ein einzelner übergroßer Record oder ein nicht passend unterzubringender
unstrukturierter Herstellerblock wird vor dem Schreiben einer Teilantwort abgewiesen.
Für die Blocknummern sind bis zu zwölf Bit vorgesehen.

SND-NKE normalisiert den Link, löscht aber keine Messdaten. Eine unterbrochene
Mehrtelegrammfolge wird mit Application Reset/Select neu gestartet. Application
Reset/Select (`50`/`53`/`57`) stellt die Standardausgabe beziehungsweise einen Block
bereit; ungültige Anwendungs-/Blockauswahl fällt auf die Standardausgabe/Block 0
zurück. Das Gerät stellt einen Datenbestand als Standardausgabe bereit.

Request of selected application (`54`/`55`/`56`) wird als SND-UD bestätigt und schaltet
die folgenden Antworten auf CI `68` um; SND-UD2 kann die erste Antwort unmittelbar
anfordern. Der Standardanwendungsinhalt besteht aus dem Anwendung-/Block-Subcode
und den codierten Antwortrecords. Der Codec liest außerdem headerlose Antworten
mit CI `66`. Application Reset/Select kehrt zum normalen RSP-UD zurück.

REQ-UD1 wird ohne implementierte Alarme mit ACK beantwortet. Ein nicht unterstützter
SND-UD2-Dienst wird nach gültigem DLL-Parsing mit NACK abgewiesen. Broadcasts führen
zu keiner Antwort. Der C#-Einstieg bleibt auf unverschlüsselte Kommunikation begrenzt;
Mode-5-Mehrtelegramme und Mode-5-Antworten der ausgewählten Anwendung bleiben
explizit nicht unterstützt. Baudrate und physisches Antworttiming gehören weiterhin
zum vom Aufrufer bereitgestellten Transport.
