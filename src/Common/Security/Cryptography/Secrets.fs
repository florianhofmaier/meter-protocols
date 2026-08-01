namespace Metering.Common.Security.Cryptography

open System

[<Sealed>]
type Secret128 private (bytes: byte[]) =

    static member TakeOwnership(bytes: byte[]) =
        ArgumentNullException.ThrowIfNull bytes

        if bytes.Length <> 16 then
            invalidArg
                (nameof bytes)
                "Key must contain exactly 16 bytes."

        Secret128 bytes

    member _.ToArray =
        bytes