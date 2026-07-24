namespace Metering.Mbus.Protocol.Frames

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.Application
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security

type CompleteMessageRaw =
    {
        Dll: Field<DllVariableLengthRaw>
        Tpl: Field<TplRaw>
    }

type FrameVariableLengthRaw =
    | CompleteMessage of CompleteMessageRaw

type CryptographicFailure =
    | DecryptionOrVerificationFailed

type UnprotectionFailure =
    | SecurityContextNotUsable
    | CryptographicFailure of CryptographicFailure

type ProtectedAplRaw =
    {
        OriginalPayload: Field<ReadOnlyMemory<byte>>
        EncryptedPart: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
        Failure: UnprotectionFailure
    }

type AplExpansionRaw =
    | Parsed of Field<AplRaw>
    | Protected of ProtectedAplRaw
    | Invalid of Failures

type CompleteMessageExpandedRaw =
    {
        Dll: Field<DllVariableLengthRaw>
        Tpl: Field<TplRaw>
        Payload: AplExpansionRaw
    }

type FrameVariableLengthExpandedRaw =
    | CompleteMessage of CompleteMessageExpandedRaw

type ProtectedApl =
    {
        OriginalPayload: Field<ReadOnlyMemory<byte>>
        EncryptedPart: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
        Failure: UnprotectionFailure
    }

type AplContent =
    | Decoded of Field<Apl>
    | Protected of ProtectedApl

type CompleteMessage =
    {
        Dll: Field<DllVariableLength>
        Tpl: Field<Tpl>
        Payload: AplContent
    }

type FrameVariableLength =
    | CompleteMessage of CompleteMessage

module private PipelineIssues =

    let single (field: Field<_>) message =
        Failures.single {
            FieldId = field.Id
            Message = message
        }

module FrameVariableLengthRaw =

    let parseCompleteMessageFromDll
        (dll: Field<DllVariableLengthRaw>)
        : Decoder<Field<FrameVariableLengthRaw>> =

        let source =
            dll.Value.UserData.Value.HigherLayerData

        if source.Value.Length > 0 && source.Value.Span[0] = 0x90uy then
            decodeError (
                ParseFailed {
                    Source = source.Span.Source
                    Pos = source.Span.Offset
                    Msg =
                        "Unsupported but standard-conformant AFL variant: CI=0x90 at HigherLayerData; "
                        + "supported path is a complete TPL message. EN 13757-7:2018, 5.2, Table 2."
                }
            )
        else
            decoder {
                let! tpl =
                    parse TplRaw.parse source

                return
                    dll
                    |> Field.withValue (
                        FrameVariableLengthRaw.CompleteMessage {
                            Dll = dll
                            Tpl = tpl
                        }
                    )
            }

module private AplParser =

    let capture
        ci
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Result<Field<AplRaw>, Failures>> =

        fun context ->
            match parse (AplRaw.parse ci) source context with
            | Metering.Common.Decoding.Decoders.Core.Decoded (value, notices) ->
                Metering.Common.Decoding.Decoders.Core.Decoded (Ok value, notices)

            | DecodeFailed (ParseFailed error, notices) ->
                let failures =
                    PipelineIssues.single
                        source
                        $"Invalid unprotected APL at byte {error.Pos}: {error.Msg}"

                Metering.Common.Decoding.Decoders.Core.Decoded (Error failures, notices)

            | DecodeFailed (failure, notices) ->
                DecodeFailed (failure, notices)

module private Mode5Expansion =

    let private blockLength = 16

    let private byteSlice
        offset
        length
        (field: Field<ReadOnlyMemory<byte>>) =

        {
            Id = field.Id
            Span = {
                field.Span with
                    Offset = field.Span.Offset + offset
                    Length = length
            }
            Value = field.Value.Slice(offset, length)
        }

    let private layout
        (cnf: Field<ConfigurationFieldBitsRaw>)
        (payload: Field<ReadOnlyMemory<byte>>) =

        let indicator =
            cnf.Value
            |> ConfigurationFieldBitsRaw.value
            |> EncryptedLengthIndicator.map

        let invalid message =
            Error (PipelineIssues.single payload message)

        match indicator with
        | NoEncryptedData ->
            Ok (byteSlice 0 0 payload, Some payload)

        | FixedEncryptedBlocks count ->
            let encryptedLength =
                EncryptedBlockCount.value count * blockLength

            if encryptedLength > payload.Value.Length then
                invalid (
                    $"Mode 5 declares {encryptedLength} encrypted byte(s), "
                    + $"but only {payload.Value.Length} payload byte(s) are available. "
                    + "EN 13757-7:2018, 7.6.5 and Tables 30/31."
                )
            else
                let encrypted =
                    byteSlice 0 encryptedLength payload

                let suffixLength =
                    payload.Value.Length - encryptedLength

                let suffix =
                    if suffixLength = 0 then None
                    else Some (byteSlice encryptedLength suffixLength payload)

                Ok (encrypted, suffix)

        | AllRemainingDataEncrypted ->
            if payload.Value.Length = 0 then
                invalid
                    "Mode 5 requires an encrypted block containing the two verification bytes."
            elif payload.Value.Length % blockLength <> 0 then
                invalid (
                    $"Mode 5 encrypted data must be a multiple of {blockLength} byte(s); "
                    + $"actual length is {payload.Value.Length}. EN 13757-7:2018, 9.4.4.1."
                )
            else
                Ok (payload, None)

    let private buildIv (header: LongHeaderMode5Raw) =
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

    let private protectedPayload original encrypted suffix failure =
        AplExpansionRaw.Protected {
            OriginalPayload = original
            EncryptedPart = encrypted
            ClearSuffix = suffix
            Failure = failure
        }

    let expand
        securityContext
        ci
        (header: LongHeaderMode5Raw)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Decoder<AplExpansionRaw> =

        match layout header.Cnf payload with
        | Error failures ->
            decodePassed (AplExpansionRaw.Invalid failures)

        | Ok (encrypted, clearSuffix) when encrypted.Value.Length = 0 ->
            decoder {
                let! parsed = AplParser.capture ci payload
                return
                    match parsed with
                    | Ok apl -> AplExpansionRaw.Parsed apl
                    | Error failures -> AplExpansionRaw.Invalid failures
            }

        | Ok (encrypted, clearSuffix) ->
            match securityContext with
            | SecurityContext.NoSecurity ->
                protectedPayload
                    payload
                    encrypted
                    clearSuffix
                    SecurityContextNotUsable
                |> decodePassed

            | SecurityContext.Mode5 mode5 ->
                let key =
                    Mode5SecurityContext.keyBytes mode5

                match AesCbc.decrypt key (buildIv header) encrypted.Value with
                | Error _ ->
                    protectedPayload
                        payload
                        encrypted
                        clearSuffix
                        (UnprotectionFailure.CryptographicFailure DecryptionOrVerificationFailed)
                    |> decodePassed

                | Ok plain
                    when plain.Length < 2
                         || plain.Span[0] <> 0x2Fuy
                         || plain.Span[1] <> 0x2Fuy ->
                    protectedPayload
                        payload
                        encrypted
                        clearSuffix
                        (UnprotectionFailure.CryptographicFailure DecryptionOrVerificationFailed)
                    |> decodePassed

                | Ok plain ->
                    let decryptedPrefix =
                        plain.Slice(2)

                    let suffix =
                        clearSuffix
                        |> Option.map (fun field -> field.Value)
                        |> Option.defaultValue ReadOnlyMemory<byte>.Empty

                    let combined =
                        Array.zeroCreate<byte> (decryptedPrefix.Length + suffix.Length)

                    decryptedPrefix.CopyTo(combined.AsMemory())
                    suffix.CopyTo(combined.AsMemory(decryptedPrefix.Length))

                    decoder {
                        let! source =
                            createDerivedSource
                                "Decrypted APL Data"
                                (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                                true
                                payload
                                (ReadOnlyMemory<byte> combined)

                        let! parsed =
                            AplParser.capture ci source

                        return
                            match parsed with
                            | Ok apl -> AplExpansionRaw.Parsed apl
                            | Error failures -> AplExpansionRaw.Invalid failures
                    }

module FrameVariableLengthExpandedRaw =

    let private expandComplete
        securityContext
        (raw: CompleteMessageRaw)
        : Decoder<CompleteMessageExpandedRaw> =

        let aplData =
            raw.Tpl.Value
            |> Tpl.rawAplData
            |> AplDataRaw.toByteField

        let ci =
            match raw.Tpl.Value with
            | TplRaw.NoneHeader tpl -> NoneTplHeader tpl.Ci.Value
            | TplRaw.ShortHeader tpl -> ShortTplHeader tpl.Ci.Value
            | TplRaw.LongHeader tpl -> LongTplHeader tpl.Ci.Value

        decoder {
            let! payload =
                match raw.Tpl.Value with
                | TplRaw.LongHeader {
                    Header = { Value = LongHeaderRaw.Mode5Raw header }
                  } ->
                    Mode5Expansion.expand securityContext ci header aplData

                | TplRaw.ShortHeader {
                    Header = { Value = ShortHeaderRaw.Mode5Raw _ }
                  } ->
                    PipelineIssues.single
                        raw.Tpl
                        "Mode 5 with a short TPL header is not supported for this wired complete-message path."
                    |> AplExpansionRaw.Invalid
                    |> decodePassed

                | _ ->
                    decoder {
                        let! parsed =
                            AplParser.capture ci aplData

                        return
                            match parsed with
                            | Ok apl -> AplExpansionRaw.Parsed apl
                            | Error failures -> AplExpansionRaw.Invalid failures
                    }

            return {
                Dll = raw.Dll
                Tpl = raw.Tpl
                Payload = payload
            }
        }

    let expand
        securityContext
        (raw: Field<FrameVariableLengthRaw>)
        : Decoder<Field<FrameVariableLengthExpandedRaw>> =

        match raw.Value with
        | FrameVariableLengthRaw.CompleteMessage complete ->
            decoder {
                let! expanded =
                    expandComplete securityContext complete

                return
                    raw
                    |> Field.withValue (
                        FrameVariableLengthExpandedRaw.CompleteMessage expanded
                    )
            }

module ProtectedApl =

    let fromRaw (raw: ProtectedAplRaw) : Validation<ProtectedApl> =
        let original = raw.OriginalPayload.Span
        let encrypted = raw.EncryptedPart.Span

        let inOriginal span =
            span.Source = original.Source
            && span.Offset >= original.Offset
            && span.Offset + span.Length <= original.Offset + original.Length

        validator {
            let! () =
                ensure
                    raw.EncryptedPart
                    "Protected encrypted range is outside the original payload."
                    (inOriginal encrypted)

            and! () =
                match raw.ClearSuffix with
                | None -> passed ()
                | Some suffix ->
                    ensure
                        suffix
                        "Protected clear-suffix range is outside the original payload."
                        (inOriginal suffix.Span
                         && suffix.Span.Offset = encrypted.Offset + encrypted.Length)

            return {
                OriginalPayload = raw.OriginalPayload
                EncryptedPart = raw.EncryptedPart
                ClearSuffix = raw.ClearSuffix
                Failure = raw.Failure
            }
        }

module AplContent =

    let fromExpandedRaw =
        function
        | AplExpansionRaw.Parsed apl ->
            Apl.fromRaw apl
            |> map AplContent.Decoded

        | AplExpansionRaw.Protected raw ->
            ProtectedApl.fromRaw raw
            |> map AplContent.Protected

        | AplExpansionRaw.Invalid failures ->
            Failed (failures, [])

module private CompleteMessageCrossLayer =

    let validateRaw (_raw: CompleteMessageExpandedRaw) =
        passed ()

module FrameVariableLength =

    let private completeFromExpandedRaw
        (raw: CompleteMessageExpandedRaw)
        : Validation<CompleteMessage> =
        validator {
            let! dll = DllVariableLength.fromRaw raw.Dll
            and! tpl = Tpl.fromRaw raw.Tpl
            and! payload = AplContent.fromExpandedRaw raw.Payload
            and! () = CompleteMessageCrossLayer.validateRaw raw

            return {
                Dll = dll
                Tpl = tpl
                Payload = payload
            }
        }

    let fromExpandedRaw
        (raw: Field<FrameVariableLengthExpandedRaw>)
        : Validation<Field<FrameVariableLength>> =

        match raw.Value with
        | FrameVariableLengthExpandedRaw.CompleteMessage complete ->
            completeFromExpandedRaw complete
            |> map (fun value ->
                raw
                |> Field.withValue (
                    FrameVariableLength.CompleteMessage value
                ))
