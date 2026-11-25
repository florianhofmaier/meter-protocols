module Metering.Common.Security.Cryptography.SensitiveBuffer

open System
open System.Security.Cryptography

/// <summary>Runs an operation over the supplied buffer and zeroes that buffer afterward, including when the operation throws; the operation sees the original bytes.</summary>
let useZeroed (buffer: byte array) (operation: ReadOnlyMemory<byte> -> 'a) : 'a =

    try
        operation (ReadOnlyMemory<byte> buffer)
    finally
        CryptographicOperations.ZeroMemory(buffer)
