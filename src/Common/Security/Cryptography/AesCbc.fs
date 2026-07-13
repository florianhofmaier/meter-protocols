module Metering.Common.Security.Cryptography.AesCbc

open System
open System.Security.Cryptography

let private blockLength =
    16

let private validateKey (key: ReadOnlyMemory<byte>) =
    match key.Length with
    | 16 | 24 | 32 -> Ok ()
    | length -> Error (InvalidKeyLength length)

let private validateIv (iv: ReadOnlyMemory<byte>) =
    if iv.Length = blockLength then Ok ()
    else Error (InvalidInitializationVectorLength iv.Length)

let private validateCipherText (cipherText: ReadOnlyMemory<byte>) =
    if cipherText.Length % blockLength = 0 then Ok ()
    else Error (CryptographicFailure $"AES-CBC ciphertext length must be a multiple of {blockLength} byte(s)")

let decrypt
    (key: ReadOnlyMemory<byte>)
    (iv: ReadOnlyMemory<byte>)
    (cipherText: ReadOnlyMemory<byte>)
    : Result<ReadOnlyMemory<byte>, EncryptionError> =

    match validateKey key, validateIv iv, validateCipherText cipherText with
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e ->
        Error e

    | Ok (), Ok (), Ok () ->
        try
            use aes =
                Aes.Create()

            aes.Mode <- CipherMode.CBC
            aes.Padding <- PaddingMode.None
            aes.Key <- key.ToArray()
            aes.IV <- iv.ToArray()

            use decryptor =
                aes.CreateDecryptor()

            let plain =
                decryptor.TransformFinalBlock(
                    cipherText.ToArray(),
                    0,
                    cipherText.Length
                )

            Ok (ReadOnlyMemory<byte> plain)

        with
        | :? CryptographicException as ex ->
            Error (CryptographicFailure ex.Message)
