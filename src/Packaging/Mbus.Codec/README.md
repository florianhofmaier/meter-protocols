# Metering.Mbus.Codec

M-Bus protocol models and stateless encoding/decoding. Requires .NET 10.
Required Common implementation assemblies are bundled in this package; there is
no separate Foundation package. The external dependency is `FSharp.Core`.

The supported consumer API for this beta is in the exact namespace `Metering.Mbus`.
Other namespaces, including `Metering.Common.*`, `Metering.Mbus.Codec` and
`Metering.Mbus.Protocol.*`, are implementation details and carry no API stability
promise, even where their types are technically public. A supported C# codec facade
is not included yet. Device-emulator applications should reference
`Metering.Mbus.Devices`; its normal API does not require F# protocol types.

The supported protocol subset is documented in the repository's M-Bus conformance
document. This package is not a full M-Bus conformance declaration.
