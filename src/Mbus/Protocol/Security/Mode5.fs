namespace Metering.Mbus.Protocol.Security

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Transport

module Mode5 =

    let private aesCheck =
        0x2Fuy

    let private encryptedBlockLength =
        16

    let private issue
        (field: ParsedField<_>)
        (message: string)
        : Issue =

        {
            FieldId = field.Id
            Message = message
        }

    let private encryptionFailed field message =
        issue field message
        |> EncryptionFailed
        |> error

    let private mapEncryptionError
        (field: ParsedField<_>)
        (error: EncryptionError)
        : DecodeFailure =

        let message =
            match error with
            | EncryptionError.InvalidKeyLength length ->
                $"invalid AES-CBC key length: {length} byte(s)"

            | EncryptionError.InvalidNonceLength length ->
                $"invalid nonce length: {length} byte(s)"

            | EncryptionError.InvalidInitializationVectorLength length ->
                $"invalid AES-CBC initialization vector length: {length} byte(s)"

            | EncryptionError.InvalidTagLength length ->
                $"invalid authentication tag length: {length} byte(s)"

            | EncryptionError.AuthenticationFailed ->
                "AES-CBC authentication failed"

            | EncryptionError.CryptographicFailure message ->
                $"AES-CBC failure: {message}"

        issue field message
        |> EncryptionFailed

    let private buildIv
        (meterAddress: MeterAddress)
        (accessNumber: AccessNumber)
        : ReadOnlyMemory<byte> =

        let iv =
            Array.zeroCreate<byte> encryptedBlockLength

        BinaryPrimitives.WriteUInt16LittleEndian(
            iv.AsSpan(0, 2),
            Manufacturer.value meterAddress.Mfr
        )

        BinaryPrimitives.WriteUInt32LittleEndian(
            iv.AsSpan(2, 4),
            IdNumber.toBcd meterAddress.IdNum
        )

        iv[6] <- Version.value meterAddress.Version
        iv[7] <- DeviceType.value meterAddress.DevType

        let acc =
            AccessNumber.value accessNumber

        iv.AsSpan(8, 8).Fill(acc)
        ReadOnlyMemory iv

    let private encryptedLength
        (cnf: ConfigurationFieldMode5)
        (aplLength: int)
        : int =

        let blocks =
            cnf.NumberOfEncryptedBlocks

        if blocks = 0uy then
            0
        elif blocks = 0x0Fuy then
            aplLength
        else
            int blocks * encryptedBlockLength

    let unprotect
        (key: ReadOnlyMemory<byte>)
        (meterAddress: MeterAddress)
        (accessNumber: AccessNumber)
        (cnf: ConfigurationFieldMode5)
        (aplData: ParsedField<ReadOnlyMemory<byte>>)
        : Decoder<ParsedField<ReadOnlyMemory<byte>>> =

        fun context ->
            let aplLength =
                aplData.Value.Length

            let encryptedLength =
                encryptedLength cnf aplLength

            match encryptedLength with
            | 0 ->
                Decoded (aplData, [])

            | encryptedLength when encryptedLength > aplLength ->
                encryptionFailed aplData $"security mode 5 encrypted length is {encryptedLength} byte(s), but APL data contains only {aplLength} byte(s)" context

            | encryptedLength when encryptedLength < aplLength ->
                encryptionFailed aplData "partial encryption is not supported" context

            | encryptedLength when encryptedLength % encryptedBlockLength <> 0 ->
                encryptionFailed aplData $"security mode 5 encrypted length must be a multiple of {encryptedBlockLength} byte(s)" context

            | encryptedLength ->
                let iv =
                    buildIv meterAddress accessNumber

                let cipherText =
                    aplData.Value.Slice(0, encryptedLength)

                match AesCbc.decrypt key iv cipherText with
                | Error error ->
                    DecodeFailed (mapEncryptionError aplData error, [])

                | Ok plain when plain.Length < 2 ->
                    encryptionFailed aplData "security mode 5 plaintext is too short for AES check" context

                | Ok plain when plain.Span[0] <> aesCheck || plain.Span[1] <> aesCheck ->
                    encryptionFailed aplData "security mode 5 AES check failed" context

                | Ok plain ->
                    let plainApl =
                        plain.Slice(2)

                    createDerivedSource
                        "Decrypted APL Data"
                        (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                        true
                        aplData
                        plainApl
                        context
