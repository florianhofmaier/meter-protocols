# MeterProtocols

In Entwicklung: .NET-Libraries für M-Bus und DLMS/COSEM.

- [Code-Standard und Formatierung](CONTRIBUTING.md)
- [Anforderungen](doc/requirements.adoc)
- [Architektur](doc/architecture/architecture.adoc)
- [Historischer Decode-Pipeline-Versuch](doc/architecture/proposals/decode-pipeline.adoc)
- [M-Bus Conformance](doc/protocols/mbus/conformance.adoc)
- [DLMS/COSEM Conformance](doc/protocols/dlms/conformance.adoc)
- [M-Bus-Gerät für C#](src/Mbus/MbusDevices/README.md)
- [Protocol Inspector: React-/TypeScript-App starten](src/Apps/ProtocolInspector/README.md)

Die gemeinsame F#-API für M-Bus liegt in `MbusCodec.dll`
(Projekt `src/Mbus/Codec/Codec.fsproj`):

```fsharp
open Metering.Mbus.Codec

let decode = Decoder.create resolver
let encode = Encoder.create options resolver
```

Beide Funktionen verarbeiten einzelne Nachrichten. Gerätezustand und Stream-I/O
liegen in `Devices`; Analyse und Diagnose liegen im eigenständigen `Analyzer`.
