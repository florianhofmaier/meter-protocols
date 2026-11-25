module Metering.Common.Security.Cryptography.AesCbc

open System
open System.Security.Cryptography

type AesCbCIv = private AesCbCIv of byte[]

module AesCbCIv =

    /// <summary>The required AES-CBC initialization vector length in bytes.</summary>
    let length = 16

    /// <summary>Validates that the initialization vector contains 16 bytes and wraps the supplied array without copying it.</summary>
    let create (iv: byte[]) =
        if iv.Length = length then
            Ok(AesCbCIv iv)
        else
            Error(EncryptionError $"Invalid initialization vectorLength: {iv.Length}")

    /// <summary>Returns the initialization vector's backing array without copying it.</summary>
    let toArray (AesCbCIv iv) = iv

module AesCbc =

    /// <summary>The AES-CBC block length in bytes.</summary>
    let blockLength = 16

    let private validateCipherText (cipherText: ReadOnlyMemory<byte>) =
        if cipherText.Length % blockLength = 0 then
            Ok()
        else
            Error(
                EncryptionError
                    $"AES-CBC ciphertext length must be a multiple of {blockLength} byte(s)"
            )

    /// <summary>Decrypts the selected ciphertext range with AES-CBC and no padding; reports invalid ciphertext-buffer alignment and cryptographic failures as encryption errors.</summary>
    let decrypt
        (key: Secret128)
        (iv: AesCbCIv)
        (cipherText: ReadOnlyMemory<byte>)
        (offset: int)
        (count: int)
        : Result<ReadOnlyMemory<byte>, EncryptionError> =

        match validateCipherText cipherText with
        | Error e -> Error e

        | Ok() ->
            try
                use aes = Aes.Create()

                aes.Mode <- CipherMode.CBC
                aes.Padding <- PaddingMode.None
                aes.Key <- key.ToArray
                aes.IV <- AesCbCIv.toArray iv

                use decryptor = aes.CreateDecryptor()

                let plain = decryptor.TransformFinalBlock(cipherText.ToArray(), offset, count)

                Ok(ReadOnlyMemory<byte> plain)

            with :? CryptographicException as ex ->
                Error(EncryptionError ex.Message)


    /// <summary>Encrypts complete plaintext blocks using AES-128-CBC without padding. Protocol-specific verification bytes and padding belong to the caller.</summary>
    let encrypt
        (key: Secret128)
        (iv: AesCbCIv)
        (plain: ReadOnlyMemory<byte>)
        : Result<ReadOnlyMemory<byte>, EncryptionError> =
        if plain.Length % blockLength <> 0 then
            Error(
                EncryptionError.create
                    $"AES-CBC plaintext length must be a multiple of {blockLength} byte(s)"
            )
        else
            let keyBytes = Array.copy key.ToArray

            try
                try
                    use aes = Aes.Create()
                    aes.Key <- keyBytes

                    let cipher =
                        aes.EncryptCbc(plain.Span, (AesCbCIv.toArray iv).AsSpan(), PaddingMode.None)

                    Ok(ReadOnlyMemory cipher)
                with :? CryptographicException as error ->
                    Error(EncryptionError.create error.Message)
            finally
                CryptographicOperations.ZeroMemory(keyBytes.AsSpan())
