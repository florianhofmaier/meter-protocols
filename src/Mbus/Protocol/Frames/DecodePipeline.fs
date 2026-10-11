namespace Metering.Mbus.Protocol.Frames.Decoding

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

/// The bytes delimited by an FT 1.2 variable-length data-link frame.
type DllUserDataRaw =
    {
        CField: Field<CFieldRaw>
        AField: Field<AFieldRaw>
        LinkUserData: Field<ReadOnlyMemory<byte>>
    }

module DllUserDataRaw =

    let parse : Parser<Field<DllUserDataRaw>> =
        parseField "DLL User Data"
        <| parser {
            let! cField = CFieldRaw.parse
            let! aField = AFieldRaw.parse
            let! linkUserData = parseField "Link User Data" takeAll

            return {
                CField = cField
                AField = aField
                LinkUserData = linkUserData
            }
        }

type DllVariableFrameRaw =
    {
        Start: Field<unit>
        Length1: Field<uint8>
        Length2: Field<uint8>
        RepeatedStart: Field<unit>
        UserData: Field<DllUserDataRaw>
        ChecksumBytes: ReadOnlyMemory<byte>
        Checksum: Field<uint8>
        End: Field<EndFieldRaw>
    }

module DllVariableFrameRaw =

    /// IEC 60870-5-2:1993, 3.2, variable-length FT 1.2 frame;
    /// EN 13757-2:2018+A1:2023, Annex F.2, wired M-Bus example.
    let parse : Parser<Field<DllVariableFrameRaw>> =
        parseField "DLL Variable Length Frame"
        <| parser {
            let! start = parseField "Start 1" (expectU8 0x68uy)
            let! length1 = parseField "Length 1" parseU8
            let! length2 = parseField "Length 2" parseU8
            let! repeatedStart = parseField "Start 2" (expectU8 0x68uy)

            let userDataLength =
                length1.Value
                |> int

            let! userDataStart = position
            let! userData = runOnSubSlice userDataLength DllUserDataRaw.parse

            let! checksumBytes =
                bufferSliceAt userDataStart userDataLength

            let! checksum = parseField "Checksum" parseU8
            let! endField = EndFieldRaw.parse

            return {
                Start = start
                Length1 = length1
                Length2 = length2
                RepeatedStart = repeatedStart
                UserData = userData
                ChecksumBytes = checksumBytes
                Checksum = checksum
                End = endField
            }
        }

type DllVariableFrame =
    {
        CField: Field<CField>
        AField: Field<AField>
        LinkUserData: Field<ReadOnlyMemory<byte>>
    }

module DllVariableFrame =

    let private validateRepeatedLength
        (length1: Field<uint8>)
        (length2: Field<uint8>) =

        ensure
            length2
            $"Length 2 must equal Length 1. Expected 0x{length1.Value:X2}, but got 0x{length2.Value:X2}."
            (length2.Value = length1.Value)

    let private validateChecksum
        (raw: Field<uint8>)
        (bytes: ReadOnlyMemory<byte>) =

        let mutable expected = 0uy

        for value in bytes.Span do
            expected <- expected + value

        ensure
            raw
            $"Checksum mismatch. Expected 0x{expected:X2}, but got 0x{raw.Value:X2}."
            (raw.Value = expected)

    let fromRaw
        (raw: Field<DllVariableFrameRaw>)
        : Validation<Field<DllVariableFrame>> =

        validator {
            let! cField =
                raw.Value.UserData.Value.CField
                |> validateField CField.fromRaw

            and! aField =
                raw.Value.UserData.Value.AField
                |> validateField AField.fromRaw

            and! () =
                validateRepeatedLength
                    raw.Value.Length1
                    raw.Value.Length2

            and! () =
                validateChecksum
                    raw.Value.Checksum
                    raw.Value.ChecksumBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = cField
                    AField = aField
                    LinkUserData = raw.Value.UserData.Value.LinkUserData
                }
        }

type HigherLayersRaw =
    {
        Ell: Field<EllRaw> option
        Nwl: Field<NwlRaw> option
        Afl: Field<AflRaw> option
        Tpl: Field<TplRaw>
    }

module HigherLayersRaw =

    /// Parses only the layer structure. Security processing and APL parsing are
    /// deliberately performed by the decoder after this parser has completed.
    let parse : Parser<Field<HigherLayersRaw>> =
        parseField "M-Bus Higher Layers"
        <| parser {
            let! ell = EllRaw.tryParse
            let! nwl = NwlRaw.tryParse
            let! afl = AflRaw.tryParse
            let! tpl = TplRaw.parse

            return {
                Ell = ell
                Nwl = nwl
                Afl = afl
                Tpl = tpl
            }
        }

type HigherLayers =
    {
        Ell: Field<Ell> option
        Tpl: Field<Tpl>
    }

module HigherLayers =

    let private validateOptional validate =
        function
        | None -> passed None
        | Some field -> validate field |> map Some

    let private requireUnsupportedLayerAbsent name =
        function
        | None -> passed ()
        | Some field ->
            failed field $"{name} validation is not implemented."

    let fromRaw
        (raw: Field<HigherLayersRaw>)
        : Validation<Field<HigherLayers>> =

        validator {
            let! ell =
                raw.Value.Ell
                |> validateOptional (validateField Ell.fromRaw)

            and! () =
                raw.Value.Nwl
                |> requireUnsupportedLayerAbsent "NWL"

            and! () =
                raw.Value.Afl
                |> requireUnsupportedLayerAbsent "AFL"

            and! tpl =
                raw.Value.Tpl
                |> validateField Tpl.fromRaw

            return
                raw
                |> Field.withValue {
                    Ell = ell
                    Tpl = tpl
                }
        }

[<RequireQualifiedAccess>]
type HigherLayersNotReached =
    | DllMalformed

[<RequireQualifiedAccess>]
type AplNotReached =
    | DllMalformed
    | HigherLayersMalformed
    | PayloadProtected

[<RequireQualifiedAccess>]
type PayloadExposureProblem =
    | UnsupportedShortHeaderMode5
    | InvalidMode5Layout of Failures
    | Mode5ContextUnavailable of UnprotectionError
    | CryptographicFailure of EncryptionError
    | VerificationFailed

type ProtectedPayload =
    {
        Original: Field<ReadOnlyMemory<byte>>
        EncryptedPrefix: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
        Problem: PayloadExposureProblem
    }

[<RequireQualifiedAccess>]
type ExposureNotAttempted =
    | DllMalformed
    | HigherLayersMalformed
    | PayloadUnavailable of ProtectedPayload

type DllStage =
    Stage<Field<DllVariableFrameRaw>, Field<DllVariableFrame>, Never>

type HigherLayersStage =
    Stage<Field<HigherLayersRaw>, Field<HigherLayers>, HigherLayersNotReached>

type PayloadExposureResult =
    Transformation<
        Field<ReadOnlyMemory<byte>>,
        ProtectedPayload,
        ExposureNotAttempted
     >

type AplStage =
    Stage<Field<AplParsedRaw>, Field<Apl>, AplNotReached>

type WiredVariableFrameDecode =
    {
        Dll: DllStage
        HigherLayers: HigherLayersStage
        Exposure: PayloadExposureResult
        Apl: AplStage
    }

module private Mode5Unprotection =

    let private blockLength = AesCbc.blockLength
    let private verificationByte = 0x2Fuy

    let private slice offset length (field: Field<ReadOnlyMemory<byte>>) =
        {
            Id = field.Id
            Span = {
                field.Span with
                    Offset = field.Span.Offset + offset
                    Length = length
            }
            Value = field.Value.Slice(offset, length)
        }

    let private invalidLayout payload message =
        failure payload message
        |> PayloadExposureProblem.InvalidMode5Layout

    let private layout
        (header: LongHeaderMode5Raw)
        (payload: Field<ReadOnlyMemory<byte>>)
        =

        let indicator =
            header.Cnf.Value
            |> ConfigurationFieldBitsRaw.value
            |> NumberOfEncryptedBlocks.map
            |> NumberOfEncryptedBlocks.value

        let encryptedLength =
            match indicator with
            | 0 -> Ok 0
            | 15 when payload.Value.Length = 0 ->
                Error (
                    invalidLayout
                        payload
                        "Mode 5 declares all remaining data encrypted, but the payload is empty. EN 13757-7:2018, 9.4.4.1, Table 48."
                )
            | 15 when payload.Value.Length % blockLength <> 0 ->
                Error (
                    invalidLayout
                        payload
                        $"Mode 5 ciphertext must be a multiple of {blockLength} byte(s); actual length is {payload.Value.Length}. EN 13757-7:2018, 9.4.4.1, Table 48."
                )
            | 15 -> Ok payload.Value.Length
            | blocks -> Ok (blocks * blockLength)

        encryptedLength
        |> Result.bind (fun length ->
            if length > payload.Value.Length then
                Error (
                    invalidLayout
                        payload
                        $"Mode 5 declares {length} encrypted byte(s), but only {payload.Value.Length} payload byte(s) are available. EN 13757-7:2018, 7.6.5 and 9.4.4.1, Table 48."
                )
            else
                let encrypted = slice 0 length payload
                let suffixLength = payload.Value.Length - length

                let suffix =
                    if suffixLength = 0 then None
                    else Some (slice length suffixLength payload)

                Ok (encrypted, suffix))

    let private protectedPayload original encrypted suffix reason =
        {
            Original = original
            EncryptedPrefix = encrypted
            ClearSuffix = suffix
            Problem = reason
        }

    let unprotect
        (resolver: IExternalSecurityContextResolver)
        (header: LongHeaderMode5Raw)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Decoder<
            Transformation<
                Field<ReadOnlyMemory<byte>>,
                ProtectedPayload,
                ExposureNotAttempted
             >> =

        match layout header payload with
        | Error reason ->
            protectedPayload payload payload None reason
            |> Transformation.Failed
            |> decodePassed

        | Ok (encrypted, suffix) when encrypted.Value.IsEmpty ->
            Transformation.Succeeded payload
            |> decodePassed

        | Ok (encrypted, suffix) ->
            let meterAddress: DeviceIdentificationRaw = {
                IdNum = header.IdNum
                Mfr = header.Mfr
                Version = header.Version
                DevType = header.DevType
            }

            match Mode5SecurityContext.create meterAddress header.Acc.Value resolver with
            | Error error ->
                PayloadExposureProblem.Mode5ContextUnavailable error
                |> protectedPayload payload encrypted suffix
                |> ExposureNotAttempted.PayloadUnavailable
                |> Transformation.NotAttempted
                |> decodePassed

            | Ok security ->
                match AesCbc.decrypt security.Key security.Iv encrypted.Value 0 encrypted.Value.Length with
                | Error error ->
                    PayloadExposureProblem.CryptographicFailure error
                    |> protectedPayload payload encrypted suffix
                    |> Transformation.Failed
                    |> decodePassed

                | Ok plain
                    when plain.Length < 2
                         || plain.Span[0] <> verificationByte
                         || plain.Span[1] <> verificationByte ->
                    PayloadExposureProblem.VerificationFailed
                    |> protectedPayload payload encrypted suffix
                    |> Transformation.Failed
                    |> decodePassed

                | Ok plain ->
                    let clearPrefix = plain.Slice(2)
                    let clearSuffix =
                        suffix
                        |> Option.map _.Value
                        |> Option.defaultValue ReadOnlyMemory<byte>.Empty

                    let combined =
                        Array.zeroCreate<byte> (clearPrefix.Length + clearSuffix.Length)

                    clearPrefix.CopyTo(combined.AsMemory())
                    clearSuffix.CopyTo(combined.AsMemory(clearPrefix.Length))

                    decoder {
                        let! derived =
                            createDerivedSource
                                "Decrypted APL Data"
                                (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                                true
                                payload
                                (ReadOnlyMemory<byte> combined)

                        return Transformation.Succeeded derived
                    }

module private PayloadExposure =

    let expose
        (resolver: IExternalSecurityContextResolver)
        (tpl: Field<TplRaw>)
        =

        let payload =
            tpl.Value
            |> TplRaw.aplData
            |> AplDataRaw.toParserSource

        match tpl.Value with
        | TplRaw.NoneHeader _
        | TplRaw.ShortHeader { Header = { Value = ShortHeaderRaw.Mode0Raw _ } }
        | TplRaw.LongHeader { Header = { Value = LongHeaderRaw.Mode0Raw _ } } ->
            Transformation.Succeeded payload
            |> decodePassed

        | TplRaw.ShortHeader { Header = { Value = ShortHeaderRaw.Mode5Raw _ } } ->
            {
                Original = payload
                EncryptedPrefix = payload
                ClearSuffix = None
                Problem = PayloadExposureProblem.UnsupportedShortHeaderMode5
            }
            |> ExposureNotAttempted.PayloadUnavailable
            |> Transformation.NotAttempted
            |> decodePassed

        | TplRaw.LongHeader { Header = { Value = LongHeaderRaw.Mode5Raw header } } ->
            Mode5Unprotection.unprotect resolver header payload

module WiredVariableFrameDecode =

    let private stageIssues
        (stage: Stage<'raw, 'valid, 'notReached>) =

        match stage with
        | Stage.Reached (_, Failed (failures, _)) -> Failures.toList failures
        | Stage.Reached (_, Passed _)
        | Stage.Malformed _
        | Stage.NotReached _ -> []

    /// All semantic validation failures from every reached stage, in decode order.
    let validationIssues report =
        let exposureIssues =
            match report.Exposure with
            | Transformation.Failed {
                Problem = PayloadExposureProblem.InvalidMode5Layout failures
              } ->
                Failures.toList failures
            | _ -> []

        stageIssues report.Dll
        @ stageIssues report.HigherLayers
        @ exposureIssues
        @ stageIssues report.Apl

    let private validateApl
        (raw: Field<AplParsedRaw>)
        : Validation<Field<Apl>> =

        Apl.fromRaw (AplRaw.Parsed raw.Value)
        |> map (fun apl -> raw |> Field.withValue apl)

    let private blockedAfterDll dll = {
        Dll = dll
        HigherLayers = Stage.NotReached HigherLayersNotReached.DllMalformed
        Exposure = Transformation.NotAttempted ExposureNotAttempted.DllMalformed
        Apl = Stage.NotReached AplNotReached.DllMalformed
    }

    let private blockedAfterHigherLayers dll higherLayers = {
        Dll = dll
        HigherLayers = higherLayers
        Exposure = Transformation.NotAttempted ExposureNotAttempted.HigherLayersMalformed
        Apl = Stage.NotReached AplNotReached.HigherLayersMalformed
    }

    /// A vertical slice proving the staged DLL -> higher layers -> exposure ->
    /// APL flow. Validation failures remain inside their reached stage.
    let decode
        (resolver: IExternalSecurityContextResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<WiredVariableFrameDecode> =

        decoder {
            let! dll =
                decodeStage
                    DllVariableFrameRaw.parse
                    DllVariableFrame.fromRaw
                    source

            match dll with
            | Stage.Malformed _ ->
                return blockedAfterDll dll

            | Stage.NotReached never ->
                return failwith $"Unreachable root-stage blocker: {never}"

            | Stage.Reached (dllRaw, _) ->
                let higherLayerSource =
                    dllRaw.Value.UserData.Value.LinkUserData

                let! higherLayers =
                    decodeStage
                        HigherLayersRaw.parse
                        HigherLayers.fromRaw
                        higherLayerSource

                match higherLayers with
                | Stage.Malformed _ ->
                    return blockedAfterHigherLayers dll higherLayers

                | Stage.NotReached reason ->
                    return failwith $"Unreachable higher-layer blocker: {reason}"

                | Stage.Reached (higherRaw, _) ->
                    let! exposure =
                        PayloadExposure.expose resolver higherRaw.Value.Tpl

                    match exposure with
                    | Transformation.Failed _
                    | Transformation.NotAttempted (ExposureNotAttempted.PayloadUnavailable _) ->
                        return {
                            Dll = dll
                            HigherLayers = higherLayers
                            Exposure = exposure
                            Apl = Stage.NotReached AplNotReached.PayloadProtected
                        }

                    | Transformation.NotAttempted reason ->
                        return failwith $"Unreachable payload exposure blocker: {reason}"

                    | Transformation.Succeeded aplSource ->
                        let ci =
                            higherRaw.Value.Tpl.Value
                            |> TplRaw.ci

                        let! apl =
                            decodeStage
                                (AplRaw.parse ci.Value)
                                validateApl
                                aplSource

                        return {
                            Dll = dll
                            HigherLayers = higherLayers
                            Exposure = exposure
                            Apl = apl
                        }
        }
