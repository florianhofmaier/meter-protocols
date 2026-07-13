namespace Metering.Dlms.Protocol.Security

open System

type GlobalCipherContext =
    {
        OriginatorSystemTitle : ReadOnlyMemory<byte>
        GlobalUnicastEncryptionKey : ReadOnlyMemory<byte>
        GlobalBroadcastEncryptionKey : ReadOnlyMemory<byte> option
        AuthenticationKey : ReadOnlyMemory<byte>
    }

type DedicatedCipherContext =
    {
        OriginatorSystemTitle : ReadOnlyMemory<byte>
        DedicatedKey : ReadOnlyMemory<byte>
        AuthenticationKey : ReadOnlyMemory<byte>
    }

type ExternalCipherContext =
    | NoCiphering
    | Global of GlobalCipherContext
    | Dedicated of DedicatedCipherContext