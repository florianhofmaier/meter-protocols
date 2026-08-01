module Metering.Common.Security.Cryptography.AesCbc

open System
open System.Security.Cryptography

type AesCbCIv =
    private AesCbCIv of byte[]

module AesCbCIv =

    let length = 16

    let create (iv: byte[]) =
        if iv.Length = length then
            Ok (AesCbCIv iv)
        else
            Error (InvalidInitializationVectorLength iv.Length)

    let toArray (AesCbCIv iv) =
        iv

module AesCbc =

    let blockLength = 16

    let private validateCipherText (cipherText: ReadOnlyMemory<byte>) =
        if cipherText.Length % blockLength = 0 then Ok ()
        else Error (CryptographicFailure $"AES-CBC ciphertext length must be a multiple of {blockLength} byte(s)")

    let decrypt
        (key: Secret128)
        (iv: AesCbCIv)
        (cipherText: ReadOnlyMemory<byte>)
        (offset: int)
        (count: int)
        : Result<ReadOnlyMemory<byte>, EncryptionError> =

        match validateCipherText cipherText with
        | Error e ->
            Error e

        | Ok () ->
            try
                use aes =
                    Aes.Create()

                aes.Mode <- CipherMode.CBC
                aes.Padding <- PaddingMode.None
                aes.Key <- key.ToArray
                aes.IV <- AesCbCIv.toArray iv

                use decryptor =
                    aes.CreateDecryptor()

                let plain =
                    decryptor.TransformFinalBlock(
                        cipherText.ToArray(),
                        offset,
                        count
                    )

                Ok (ReadOnlyMemory<byte> plain)

            with
            | :? CryptographicException as ex ->
                Error (CryptographicFailure ex.Message)
