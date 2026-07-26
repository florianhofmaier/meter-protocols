namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type CryptographicFailure =
    | DecryptionOrVerificationFailed

type UnprotectionFailure =
    | SecurityContextNotUsable
    | CryptographicFailure of CryptographicFailure

type Mode5ProtectedLayout =
    {
        OriginalPayload: Field<ReadOnlyMemory<byte>>
        EncryptedPart: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
    }

type Mode5ExpansionOutcome =
    | Unprotected of Field<ReadOnlyMemory<byte>>
    | Protected of Mode5ProtectedLayout * UnprotectionFailure
    | Invalid of Failures

module Mode5 =

    let private blockLength = 16
    let private aesCheck = 0x2Fuy

    let private issue (field: Field<_>) message =
        Failures.single {
            FieldId = field.Id
            Message = message
        }

    let private byteSlice offset length (field: Field<ReadOnlyMemory<byte>>) =
        {
            Id = field.Id
            Span = {
                field.Span with
                    Offset = field.Span.Offset + offset
                    Length = length
            }
            Value = field.Value.Slice(offset, length)
        }

    let private protectedLayout
        (cnf: Field<ConfigurationFieldBitsRaw>)
        (payload: Field<ReadOnlyMemory<byte>>) =

        let invalid message =
            Error (issue payload message)

        let withEncryptedLength encryptedLength =
            if encryptedLength > payload.Value.Length then
                invalid (
                    $"Mode 5 declares {encryptedLength} encrypted byte(s), "
                    + $"but only {payload.Value.Length} payload byte(s) are available. "
                    + "EN 13757-7:2018, 7.6.5 and 7.7.4, Tables 30/31."
                )
            else
                let encrypted =
                    byteSlice 0 encryptedLength payload

                let suffixLength =
                    payload.Value.Length - encryptedLength

                let suffix =
                    if suffixLength = 0 then None
                    else Some (byteSlice encryptedLength suffixLength payload)

                Ok {
                    OriginalPayload = payload
                    EncryptedPart = encrypted
                    ClearSuffix = suffix
                }

        let indicator =
            cnf.Value
            |> ConfigurationFieldBitsRaw.value
            |> EncryptedLengthIndicator.map

        match indicator with
        | NoEncryptedData ->
            withEncryptedLength 0

        | FixedEncryptedBlocks count ->
            count
            |> EncryptedBlockCount.value
            |> (*) blockLength
            |> withEncryptedLength

        | AllRemainingDataEncrypted ->
            if payload.Value.Length = 0 then
                invalid
                    "Mode 5 requires an encrypted block containing the two decryption-verification bytes."
            elif payload.Value.Length % blockLength <> 0 then
                invalid (
                    $"Mode 5 encrypted data must be a multiple of {blockLength} byte(s); "
                    + $"actual length is {payload.Value.Length}. EN 13757-7:2018, 9.4.4.1."
                )
            else
                withEncryptedLength payload.Value.Length

    let private buildLongHeaderIv (header: LongHeaderMode5Raw) =
        let iv = Array.zeroCreate<byte> blockLength

        BinaryPrimitives.WriteUInt16LittleEndian(
            iv.AsSpan(0, 2),
            ManufacturerRaw.value header.Mfr.Value
        )

        BinaryPrimitives.WriteUInt32LittleEndian(
            iv.AsSpan(2, 4),
            IdNumberRaw.value header.IdNum.Value
        )

        iv[6] <- VersionRaw.value header.Version.Value
        iv[7] <- DeviceTypeRaw.value header.DevType.Value
        iv.AsSpan(8, 8).Fill(AccessNumberRaw.value header.Acc.Value)
        ReadOnlyMemory<byte> iv

    let private decrypt
        securityContext
        iv
        (layout: Mode5ProtectedLayout)
        : Decoder<Mode5ExpansionOutcome> =

        if layout.EncryptedPart.Value.Length = 0 then
            decodePassed (Unprotected layout.OriginalPayload)
        else
            match securityContext with
            | SecurityContext.NoSecurity ->
                decodePassed (
                    Protected (
                        layout,
                        UnprotectionFailure.SecurityContextNotUsable
                    )
                )

            | SecurityContext.Mode5 mode5 ->
                let key =
                    Mode5SecurityContext.keyBytes mode5

                match AesCbc.decrypt key iv layout.EncryptedPart.Value with
                | Error _ ->
                    decodePassed (
                        Protected (
                            layout,
                            UnprotectionFailure.CryptographicFailure
                                DecryptionOrVerificationFailed
                        )
                    )

                | Ok plain
                    when plain.Length < 2
                         || plain.Span[0] <> aesCheck
                         || plain.Span[1] <> aesCheck ->
                    decodePassed (
                        Protected (
                            layout,
                            UnprotectionFailure.CryptographicFailure
                                DecryptionOrVerificationFailed
                        )
                    )

                | Ok plain ->
                    let decryptedPrefix =
                        plain.Slice(2)

                    let clearSuffix =
                        layout.ClearSuffix
                        |> Option.map (fun suffix -> suffix.Value)
                        |> Option.defaultValue ReadOnlyMemory<byte>.Empty

                    let combined =
                        Array.zeroCreate<byte>
                            (decryptedPrefix.Length + clearSuffix.Length)

                    decryptedPrefix.CopyTo(combined.AsMemory())
                    clearSuffix.CopyTo(combined.AsMemory(decryptedPrefix.Length))

                    decoder {
                        let! source =
                            createDerivedSource
                                "Decrypted APL Data"
                                (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                                true
                                layout.OriginalPayload
                                (ReadOnlyMemory<byte> combined)

                        return Unprotected source
                    }

    let expandLongHeader
        securityContext
        (header: LongHeaderMode5Raw)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Decoder<Mode5ExpansionOutcome> =

        match protectedLayout header.Cnf payload with
        | Error failures ->
            decodePassed (Invalid failures)

        | Ok layout ->
            decrypt
                securityContext
                (buildLongHeaderIv header)
                layout
