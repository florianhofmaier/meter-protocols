# M-Bus Beta 0.1.0-beta.2

Für einen C#-Geräteemulator wird nur **Metering.Mbus.Devices** installiert.
Benötigt wird .NET 10. Die Pakete werden zunächst als lokaler NuGet-Feed unter
`artifacts/nuget` im Repository-Root erzeugt. Das Release-Skript veröffentlicht
nichts auf nuget.org oder GitHub und verändert keine NuGet-Quellen.

```text
Metering.Mbus.Devices
  └─ Metering.Mbus.Codec
       └─ FSharp.Core
```

`Metering.Mbus.Codec` enthält die erforderlichen Protocol-, PublicTypes- und
Common-Assemblies. Es gibt kein separates Foundation-NuGet. Eine Assembly wird
genau einem Paket zugeordnet; Devices enthält die F#-Gerätelogik und die C#-Fassade.
Beide eigenen Pakete haben dieselbe Beta-Version.

## Verwendung

Den lokalen Feed zusätzlich zu nuget.org registrieren (absoluten Pfad anpassen):

```sh
dotnet nuget add source /pfad/zu/MeterProtocols/artifacts/nuget --name MeterProtocols-local
dotnet add package Metering.Mbus.Devices --version 0.1.0-beta.2
```

```csharp
using Metering.Mbus;

var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);
device.UpdateUserData(data =>
    data.AddInt32(1234, record => record.AsCubicMetersExpMinus3()));

await device.RunAsync(stream, primaryAddress: 5, cancellationToken);
```

Der Aufrufer stellt einen les- und schreibbaren Duplex-Stream bereit und verwaltet
seinen Lebenszyklus. Die Anbindung an Serial-Port, TCP oder ein anderes Transportmedium
liegt beim Anwender. Einzelheiten zu Updates, eigenen `IMbusUserDataBuilder`-Implementierungen,
Telegrammfolgen und unterstützten Records stehen in der
[Device-Dokumentation](../Mbus/MbusDevices/README.md).

## Öffentlicher API-Vertrag

Die unterstützte öffentliche API dieser Beta liegt im **exakten Namespace
`Metering.Mbus`**. Andere Namespaces, insbesondere `Metering.Common.*`,
`Metering.Mbus.Codec` und `Metering.Mbus.Protocol.*`, sind Implementierungsdetails.
Technische `public`-Sichtbarkeit macht sie nicht zu einem unterstützten Vertrag.
Die C#-Device-Signaturen verlangen keine Common- oder internen F#-Modelltypen.
Eine unterstützte C#-Codec-Fassade ist noch nicht Teil dieser Beta.

Die API kann sich zwischen Beta-Versionen ändern. Als Paket-ID ist Codec bereits
für die langfristige Aufteilung reserviert; der aktuelle Paketname ist keine
Stabilitätszusage für dessen technische Namespaces.

## Beta erzeugen und prüfen

Voraussetzungen: das SDK aus `global.json`, .NET-9-Runtime für bestehende Common-Tests,
Python 3, Git und Zugriff auf die benötigten NuGet-Abhängigkeiten. Vom Repository-Root:

```sh
bash src/Packaging/release.sh
```

Bei mehreren SDK-Installationen kann `DOTNET` auf die gewünschte ausführbare Datei
zeigen. `BETA_NUGET_SOURCE` ersetzt bei Bedarf nuget.org durch einen anderen Feed.
Der zum SDK gehörende FSharp-Core-Feed wird zusätzlich berücksichtigt.

Das Skript führt die Common- und M-Bus-Tests in Release aus, erstellt beide Pakete
und prüft Paketabhängigkeiten, enthaltene DLLs und den Git-Commit in den Metadaten
sowie MIT-Lizenzangabe, Copyright und den mitgelieferten Lizenztext.
Anschließend wird ein C#-Consumer mit genau einer Paketreferenz in ein eigenes
Verzeichnis kopiert und mit leerem Paketcache wiederhergestellt. Er prüft die
Emulation über einen Duplex-Stream, FCB-Wiederholung, Mehrtelegramme, Datenupdates,
einen eigenen Builder, Fehler bei Updates und Abbruch über CancellationToken.

Ergebnisse liegen unter `artifacts/test-results/mbus-beta`, Pakete unter
`artifacts/nuget`, kopierte Consumer unter `artifacts/consumer`. Diese Verzeichnisse
sind von Git ausgeschlossen. Ein erneutes Packen nach bereits erfolgreichen Tests
ist mit `--skip-tests` möglich; Paketprüfung und Consumer laufen weiterhin.
Die Version wird zentral in [Version.props](Version.props) gepflegt.

## Umfang und Grenzen

Diese Beta dient der unverschlüsselten wired M-Bus-Geräteemulation. Sie enthält
FCB-Verarbeitung, feste Datenbestände während einer Mehrtelegramm-Auslesung und
die implementierten Application-/Block-Dienste. Die bestehende Testabdeckung und
offene Normbereiche stehen in der
[Conformance-Dokumentation](../../doc/protocols/mbus/conformance.adoc).

Die Fluent-Builder-API deckt noch nicht alle Standarddarstellungen ab; die
Device-Dokumentation führt die konkreten Grenzen auf. Ein einzelner Record oder
unstrukturierter Herstellerblock muss in eine Antwort passen. Ein vollständiger
RSP-UD-Byte-Import, verschlüsselte C#-Geräteausgabe und Wireless-Geräteemulation
sind nicht enthalten. Physisches Timing und Baudrate sind Aufgabe des Transports.
DLMS, Analyzer und Protocol Inspector werden nicht mit ausgeliefert.

## Lizenz

Das Projekt steht unter der [MIT-Lizenz](../../LICENSE).
Copyright (c) 2026 Florian Hofmaier. Beide NuGet-Pakete enthalten den vollständigen
Lizenztext als `LICENSE` und die SPDX-Lizenzangabe `MIT` in ihren Metadaten.
Abhängigkeiten behalten ihre jeweiligen Lizenzen.

Beta `0.1.0-beta.2` ergänzt die Lizenz gegenüber `0.1.0-beta.1`.
Die Device-API und Protokollimplementierung sind unverändert.
