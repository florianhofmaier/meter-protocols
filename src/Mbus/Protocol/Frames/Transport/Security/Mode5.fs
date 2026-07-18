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

    let private validateCtx
        (field: Field<_>)
        (securityContext: SecurityContext)
        : Validation<Mode5SecurityContext> =

        match securityContext with
        | SecurityContext.Mode5 ctx -> passed ctx
        | _ -> failed field "security mode 5 context is required"

    let private issue
        (field: Field<_>)
        (message: string)
        : Issue =

        {
            FieldId = field.Id
            Message = message
        }

    let private encryptionFailed field message =
        issue field message
        |> EncryptionFailed
        |> decodeError

    let private mapEncryptionError
        (field: Field<_>)
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
        (meterAddress: DeviceIdentification)
        (accessNumber: AccessNumber)
        : ReadOnlyMemory<byte> =

        let iv =
            Array.zeroCreate<byte> encryptedBlockLength

        BinaryPrimitives.WriteUInt16LittleEndian(
            iv.AsSpan(0, 2),
            Manufacturer.value meterAddress.Mfr.Value
        )

        BinaryPrimitives.WriteUInt32LittleEndian(
            iv.AsSpan(2, 4),
            IdNumber.toBcd meterAddress.IdNum.Value
        )

        iv[6] <- Version.value meterAddress.Version.Value
        iv[7] <- DeviceType.value meterAddress.DevType.Value

        let acc =
            AccessNumber.value accessNumber

        iv.AsSpan(8, 8).Fill(acc)
        ReadOnlyMemory iv

    let private validateEncryptedLength
        (cnf: Field<ConfigurationFieldMode5>)
        (aplData: Field<ReadOnlyMemory<byte>>)
        : Validation<int> =

        let availableLength =
            aplData.Value.Length

        match cnf.Value.EncryptedLength with
        | NoEncryptedData ->
            failed
                aplData
                "Security mode 5 with no encrypted data is standard-defined but currently unsupported."

        | FixedEncryptedBlocks blockCount ->
            let encryptedLength =
                blockCount
                |> EncryptedBlockCount.value
                |> (*) encryptedBlockLength

            if encryptedLength > availableLength then
                failed
                    aplData
                    $"Declared encrypted length is {encryptedLength} byte(s), but only {availableLength} byte(s) are available."
            elif encryptedLength < availableLength then
                failed
                    aplData
                    $"Security mode 5 partial encryption is standard-defined but currently unsupported. Declared encrypted length is {encryptedLength} byte(s), available payload is {availableLength} byte(s)."
            else
                passed encryptedLength

        | AllRemainingDataEncrypted ->
            if availableLength = 0 then
                failed
                    aplData
                    "mode 5 requires at least one encrypted block containing decryption-verification bytes."
            elif availableLength % encryptedBlockLength <> 0 then
                failed
                    aplData
                    $"Security mode 5 all-remaining encrypted payload length must be a multiple of {encryptedBlockLength} byte(s), but got {availableLength} byte(s)."
            else
                passed availableLength

    let unprotect
        (ctx: SecurityContext)
        (meterAddress: DeviceIdentification)
        (acc: Field<AccessNumber>)
        (cnf: Field<ConfigurationFieldMode5>)
        (aplData: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<ReadOnlyMemory<byte>>> =

        decoder {
            let! mode5 =
                validate (fun field -> validateCtx field ctx) aplData

            let! encryptedLength =
                validate (validateEncryptedLength cnf) aplData

            let iv =
                buildIv meterAddress acc.Value

            let cipherText =
                aplData.Value.Slice(0, encryptedLength)

            let key =
                Mode5SecurityContext.keyBytes mode5

            match AesCbc.decrypt key iv cipherText with
            | Error error ->
                return!
                    decodeError (mapEncryptionError aplData error)

            | Ok plain when plain.Length < 2 ->
                return!
                    encryptionFailed
                        aplData
                        "security mode 5 plaintext is too short for AES check"

            | Ok plain when plain.Span[0] <> aesCheck || plain.Span[1] <> aesCheck ->
                return!
                    encryptionFailed
                        aplData
                        "security mode 5 AES check failed"

            | Ok plain ->
                let applicationBytes =
                    plain.Slice(2)

                return!
                    createDerivedSource
                        "Decrypted APL Data"
                        (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                        true
                        aplData
                        applicationBytes
        }
