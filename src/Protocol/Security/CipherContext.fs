namespace Metering.Dlms.Protocol.Security

open System

type ExternalCipherContext =
    {
        OriginatorSystemTitle : ReadOnlyMemory<byte>
        DedicatedKey : ReadOnlyMemory<byte>
        AuthenticationKey : ReadOnlyMemory<byte>
    }