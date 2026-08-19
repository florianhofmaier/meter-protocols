module Metering.Common.Security.Cryptography.AesGcm

open System
open System.Security.Cryptography

let private tagLength =
    12

let private validateKey (key: ReadOnlyMemory<byte>) =
    match key.Length with
    | 16 | 24 | 32 -> Ok ()
    | length -> Error (EncryptionError $"Invalid key length: {length}")

let private validateNonce (nonce: ReadOnlyMemory<byte>) =
    if nonce.Length = 12 then Ok ()
    else Error (EncryptionError $"Invalid nonce length: {nonce.Length}")

let private validateTag (tag: ReadOnlyMemory<byte>) =
    if tag.Length = tagLength then Ok ()
    else Error (EncryptionError $"Invalid tag length: {tag.Length}")

let decrypt
    (key: ReadOnlyMemory<byte>)
    (nonce: ReadOnlyMemory<byte>)
    (associatedData: ReadOnlyMemory<byte>)
    (cipherText: ReadOnlyMemory<byte>)
    (tag: ReadOnlyMemory<byte>)
    : Result<ReadOnlyMemory<byte>, EncryptionError> =

    match validateKey key, validateNonce nonce, validateTag tag with
    | Error e, _, _
    | _, Error e, _
    | _, _, Error e ->
        Error e

    | Ok (), Ok (), Ok () ->
        let plain =
            Array.zeroCreate<byte> cipherText.Length

        try
            use aes =
                new AesGcm(key.Span, tagLength)

            aes.Decrypt(
                nonce.Span,
                cipherText.Span,
                tag.Span,
                plain.AsSpan(),
                associatedData.Span
            )

            Ok (ReadOnlyMemory<byte> plain)

        with
        | :? AuthenticationTagMismatchException ->
            CryptographicOperations.ZeroMemory(plain)
            Error (EncryptionError "AuthenticationFailed")

        | :? CryptographicException as ex ->
            CryptographicOperations.ZeroMemory(plain)
            Error (EncryptionError $"Decryption failed: {ex.Message}")

let verifyTag
    (key: ReadOnlyMemory<byte>)
    (nonce: ReadOnlyMemory<byte>)
    (associatedData: ReadOnlyMemory<byte>)
    (tag: ReadOnlyMemory<byte>)
    : Result<unit, EncryptionError> =

    match decrypt key nonce associatedData ReadOnlyMemory<byte>.Empty tag with
    | Ok _ ->
        Ok ()

    | Error error ->
        Error error