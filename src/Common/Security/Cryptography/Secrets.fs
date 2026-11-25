namespace Metering.Common.Security.Cryptography

open System

[<Sealed>]
type Secret128 private (bytes: byte[]) =

    /// <summary>Creates a 128-bit secret using the supplied array without copying; throws for null or a length other than 16 bytes.</summary>
    static member TakeOwnership(bytes: byte[]) =
        ArgumentNullException.ThrowIfNull bytes

        if bytes.Length <> 16 then
            invalidArg (nameof bytes) "Key must contain exactly 16 bytes."

        Secret128 bytes

    /// <summary>Returns the secret's backing array without copying; modifications affect the stored secret.</summary>
    member _.ToArray = bytes
