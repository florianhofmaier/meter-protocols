module Metering.Dlms.Protocol.Xdlms.Security.GloCiphering

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Common.Utility.Result
open Metering.Dlms.Protocol.Security
open Metering.Dlms.Protocol.Security.ProtectedApdus

let private toByteField
    (source: Field<_>)
    (bytes: ReadOnlyMemory<byte>)
    : Field<ReadOnlyMemory<byte>> =
    {
        Id = source.Id
        Span = source.Span
        Value = bytes
    }

let private buildIv
    (systemTitle: ReadOnlyMemory<byte>)
    (invocationCounter: InvocationCounter)
    : byte array =

    let ic =
        InvocationCounter.toBytes invocationCounter

    let iv =
        Array.zeroCreate<byte> 12

    systemTitle.Span.CopyTo(iv.AsSpan(0, 8))
    ic.Span.CopyTo(iv.AsSpan(8, 4))

    iv

let private buildAuthenticatedEncryptionAad
    (securityControl: SecurityControl)
    (authenticationKey: ReadOnlyMemory<byte>)
    : byte array =

    let aad =
        Array.zeroCreate<byte> (1 + authenticationKey.Length)

    aad[0] <- SecurityControl.toByte securityControl

    authenticationKey.Span.CopyTo(aad.AsSpan(1))

    aad

let private buildAuthenticationOnlyAad
    (securityControl: SecurityControl)
    (authenticationKey: ReadOnlyMemory<byte>)
    (information: ReadOnlyMemory<byte>)
    : byte array =

    let aad =
        Array.zeroCreate<byte>
            (1 + authenticationKey.Length + information.Length)

    aad[0] <- SecurityControl.toByte securityControl

    authenticationKey.Span.CopyTo(
        aad.AsSpan(1, authenticationKey.Length)
    )

    information.Span.CopyTo(
        aad.AsSpan(1 + authenticationKey.Length, information.Length)
    )

    aad

let private authenticationTagLength = 12

let private issue
    (field: Field<_>)
    (message: string)
    : Issue =
    {
        FieldId = field.Id
        Message = message
    }


let private mapAesGcmError
    (field: Field<_>)
    (error: EncryptionError)
    : Issue =

    let message =
        match error with
        | EncryptionError.InvalidKeyLength length ->
            $"invalid AES-GCM key length: {length} byte(s)"

        | EncryptionError.InvalidNonceLength length ->
            $"invalid AES-GCM nonce length: {length} byte(s)"

        | EncryptionError.InvalidInitializationVectorLength length ->
            $"invalid initialization vector length: {length} byte(s)"

        | EncryptionError.InvalidTagLength length ->
            $"invalid AES-GCM authentication tag length: {length} byte(s)"

        | EncryptionError.AuthenticationFailed ->
            "AES-GCM authentication failed"

        | EncryptionError.CryptographicFailure message ->
            $"AES-GCM failure: {message}"

    issue field message


let private concat
    (parts: byte array list)
    : byte array =
    parts |> Array.concat


let private splitAuthenticationTag
    (field: Field<ReadOnlyMemory<byte>>)
    : Result<ReadOnlyMemory<byte> * ReadOnlyMemory<byte>, Issue> =

    let bytes =
        field.Value

    if bytes.Length < authenticationTagLength then
        Error (
            issue
                field
                $"protected payload is too short; expected at least {authenticationTagLength} byte authentication tag"
        )
    else
        let contentLength =
            bytes.Length - authenticationTagLength

        Ok (
            bytes.Slice(0, contentLength),
            bytes.Slice(contentLength, authenticationTagLength)
        )

let private selectEncryptionKey
    (cipherContext: GlobalCipherContext)
    (securityControl: SecurityControl)
    (field: Field<_>)
    : Result<ReadOnlyMemory<byte>, Issue> =

    match securityControl.KeySet with
    | KeySet.Unicast ->
        Ok cipherContext.GlobalUnicastEncryptionKey

    | KeySet.Broadcast ->
        match cipherContext.GlobalBroadcastEncryptionKey with
        | Some key ->
            Ok key

        | None ->
            Error (
                issue
                    field
                    "Security Control selects broadcast key set, but no global broadcast encryption key is available"
            )

let private unprotectAuthenticationOnly
    (cipherContext: GlobalCipherContext)
    (encryptionKey: ReadOnlyMemory<byte>)
    (iv: ReadOnlyMemory<byte>)
    (securityControl: SecurityControl)
    (payload: Field<ReadOnlyMemory<byte>>)
    : Result<Field<ReadOnlyMemory<byte>>, Issue> =

    result {
        let! information, authenticationTag =
            splitAuthenticationTag payload

        let aad =
            buildAuthenticationOnlyAad
                securityControl
                cipherContext.AuthenticationKey
                information

        do!
            SensitiveBuffer.useZeroed aad (fun aad ->
                AesGcm.verifyTag
                    encryptionKey
                    iv
                    aad
                    authenticationTag
            )
            |> Result.mapError (mapAesGcmError payload)

        return
            payload
            |> Field.map (fun _ -> information)
    }

let private unprotectAuthenticatedEncryption
    (cipherContext: GlobalCipherContext)
    (encryptionKey: ReadOnlyMemory<byte>)
    (iv: ReadOnlyMemory<byte>)
    (securityControl: SecurityControl)
    (payload: Field<ReadOnlyMemory<byte>>)
    : Result<Field<ReadOnlyMemory<byte>>, Issue> =

    result {
        let! cipherText, authenticationTag =
            splitAuthenticationTag payload

        let aad =
            buildAuthenticatedEncryptionAad
                securityControl
                cipherContext.AuthenticationKey

        let! plain =
            SensitiveBuffer.useZeroed aad (fun aad ->
                AesGcm.decrypt
                    encryptionKey
                    iv
                    aad
                    cipherText
                    authenticationTag
            )
            |> Result.mapError (mapAesGcmError payload)

        return
            payload
            |> Field.map (fun _ -> plain)
    }

let private unprotectServiceSpecific
    (cipherContext: GlobalCipherContext)
    (protectedApdu: Field<ProtectedApduValidatedFields>)
    : Result<Field<ReadOnlyMemory<byte>>, Issue> =

    result {
        let securityControl =
            protectedApdu.Value.SecurityControl.Value

        let payload =
            protectedApdu.Value.Payload

        do!
            if securityControl.CompressionApplied then
                Error (
                    issue
                        payload
                        "compression is not supported for service-specific glo-ciphering"
                )
            else
                Ok ()

        let! encryptionKey =
            selectEncryptionKey
                cipherContext
                securityControl
                payload

        let iv =
            buildIv
                cipherContext.OriginatorSystemTitle
                protectedApdu.Value.InvocationCounter.Value

        return!
            SensitiveBuffer.useZeroed iv (fun iv ->

                match securityControl.ProtectionMode with
                | ProtectionMode.NoProtection ->
                    Error (
                        issue
                            payload
                            "glo-ciphered APDU has neither authentication nor encryption applied"
                    )

                | ProtectionMode.AuthenticationOnly ->
                    unprotectAuthenticationOnly
                        cipherContext
                        encryptionKey
                        iv
                        securityControl
                        payload

                | ProtectionMode.EncryptionOnly ->
                    Error (
                        issue
                            payload
                            "encryption-only AES-GCM is not implemented"
                    )

                | ProtectionMode.AuthenticatedEncryption ->
                    unprotectAuthenticatedEncryption
                        cipherContext
                        encryptionKey
                        iv
                        securityControl
                        payload
            )
    }

let unprotect
    (cipherContext: GlobalCipherContext)
    (protectedApdu: Field<ProtectedApduValidatedFields>)
    : Decoder<Field<ReadOnlyMemory<byte>>> =

    fun _ ->
        match unprotectServiceSpecific cipherContext protectedApdu with
        | Ok plain ->
            Decoded (plain, [])

        | Error failure ->
            DecodeFailed (EncryptionFailed failure, [])
