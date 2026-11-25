# Metering.Mbus.Devices

Beta C# API for wired M-Bus device emulation on .NET 10. Install this package;
NuGet automatically restores `Metering.Mbus.Codec` and `FSharp.Core`. Required
Common DLLs are bundled in the Codec package, without a separate Foundation NuGet.

```csharp
using Metering.Mbus;

var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);
device.UpdateUserData(data =>
    data.AddInt32(1234, record => record.AsCubicMetersExpMinus3()));

await device.RunAsync(stream, primaryAddress: 5, cancellationToken);
```

The caller supplies and owns a readable/writable duplex stream. Updates replace
the whole data set atomically. Failed updates report errors at the update call
and preserve the previous data. A multi-telegram readout keeps a fixed data set.

Custom `IMbusUserDataBuilder` implementations can supply complete encoded response
APL bytes through `byte[] Build()`, passed to `device.UpdateUserData(builder)`.
The device copies, parses and validates the bytes before accepting them.

This beta supports unencrypted wired communication, FCB handling, multi-telegram
readout and the implemented application/block selection services. Individual
records and unstructured manufacturer blocks must fit in a response. Physical bus
timing and baud-rate configuration belong to the supplied transport. Wireless
M-Bus and encrypted multi-telegram output are outside this beta's scope.

The supported public API is the exact namespace `Metering.Mbus`. Other namespaces
are implementation details, even if their types are technically public. This API
and the supported subset can change between beta releases. No DLMS,
Analyzer or Protocol Inspector assemblies are included.

Licensed under the MIT License. Copyright (c) 2026 Florian Hofmaier.
The complete license text is included as `LICENSE` in this package.
