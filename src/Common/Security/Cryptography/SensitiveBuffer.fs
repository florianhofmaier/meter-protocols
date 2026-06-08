module Metering.Common.Security.Cryptography.SensitiveBuffer

open System
open System.Security.Cryptography

let useZeroed
    (buffer: byte array)
    (operation: ReadOnlyMemory<byte> -> 'a)
    : 'a =

    try
        operation (ReadOnlyMemory<byte> buffer)
    finally
        CryptographicOperations.ZeroMemory(buffer)