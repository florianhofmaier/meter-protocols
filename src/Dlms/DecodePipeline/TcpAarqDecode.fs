namespace Metering.Dlms.Protocol.CosemApdus.Decoding

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Aarq
open Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp
open Metering.Dlms.Protocol.Security
open Metering.Dlms.Protocol.Security.ProtectedApdus
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.InitiateRequest
open Metering.Dlms.Protocol.Xdlms.Security

/// Key material supplied from outside the decoded message.
type GlobalCipherKeys =
    {
        GlobalUnicastEncryptionKey: ReadOnlyMemory<byte>
        GlobalBroadcastEncryptionKey: ReadOnlyMemory<byte> option
        AuthenticationKey: ReadOnlyMemory<byte>
    }

/// Resolves external key material using the originator system title read from
/// the current AARQ. The system title itself is deliberately not part of the
/// returned external security context.
type IGlobalCipherKeyResolver =
    abstract ResolveGlobalKeys:
        originatorSystemTitle: ReadOnlyMemory<byte> -> GlobalCipherKeys option

[<RequireQualifiedAccess>]
type AarqNotReached =
    | WrapperMalformed

[<RequireQualifiedAccess>]
type ProtectedApduNotReached =
    | WrapperMalformed
    | AarqMalformed
    | UserInformationMissing

[<RequireQualifiedAccess>]
type InitiateRequestNotReached =
    | WrapperMalformed
    | AarqMalformed
    | UserInformationMissing
    | ProtectedApduMalformed
    | PayloadProtected

[<RequireQualifiedAccess>]
type UnprotectionProblem =
    | InvalidProtectedApdu of Failures
    | OriginatorSystemTitleMissing
    | InvalidOriginatorSystemTitleLength of actual: int
    | GlobalKeysUnavailable
    | CryptographicFailure of Issue

type RetainedProtectedInitiateRequest =
    {
        Envelope: Field<GloInitiateRequestRaw>
        Problem: UnprotectionProblem
    }

[<RequireQualifiedAccess>]
type ExposureNotAttempted =
    | WrapperMalformed
    | AarqMalformed
    | UserInformationMissing
    | ProtectedApduMalformed
    | PayloadUnavailable of RetainedProtectedInitiateRequest

type WrapperStage =
    Stage<Field<WrapperPduRaw>, Field<WrapperPdu>, Never>

type AarqStage =
    Stage<Field<AarqRaw>, AarqValidatedFields, AarqNotReached>

type ProtectedApduStage =
    Stage<
        Field<GloInitiateRequestRaw>,
        GloInitiateRequestValidatedFields,
        ProtectedApduNotReached
     >

type PayloadExposure =
    Transformation<
        Field<ReadOnlyMemory<byte>>,
        RetainedProtectedInitiateRequest,
        ExposureNotAttempted
     >

type InitiateRequestStage =
    Stage<
        Field<InitiateRequestRaw>,
        InitiateRequestValidatedFields,
        InitiateRequestNotReached
     >

type TcpAarqDecodeReport =
    {
        Wrapper: WrapperStage
        Aarq: AarqStage
        ProtectedApdu: ProtectedApduStage
        Exposure: PayloadExposure
        InitiateRequest: InitiateRequestStage
    }

module TcpAarqDecode =

    let private stageIssues
        (stage: Stage<'raw, 'valid, 'notReached>) =

        match stage with
        | Stage.Reached (_, Failed (failures, _)) -> Failures.toList failures
        | Stage.Reached (_, Passed _)
        | Stage.Malformed _
        | Stage.NotReached _ -> []

    /// All semantic validation failures from reached structural stages, in
    /// decode order. Transformation failures remain on the transformation.
    let validationIssues report =
        stageIssues report.Wrapper
        @ stageIssues report.Aarq
        @ stageIssues report.ProtectedApdu
        @ stageIssues report.InitiateRequest

    let private userInformationSource
        (aarq: Field<AarqRaw>)
        : Field<ReadOnlyMemory<byte>> option =

        aarq.Value.UserInformation.Value
        |> Option.map (fun octets ->
            let bytes = Ber.OctetString.value octets

            // user-information is encoded as an explicit ACSE field containing
            // a BER OCTET STRING. Its value is the final bytes of the field.
            {
                Id = aarq.Value.UserInformation.Id
                Span = {
                    aarq.Value.UserInformation.Span with
                        Offset =
                            aarq.Value.UserInformation.Span.Offset
                            + aarq.Value.UserInformation.Span.Length
                            - bytes.Length
                        Length = bytes.Length
                }
                Value = bytes
            })

    let private originatorSystemTitle
        (aarq: Field<AarqRaw>)
        : Result<ReadOnlyMemory<byte>, UnprotectionProblem> =

        match aarq.Value.CallingApTitle.Value with
        | None ->
            Error UnprotectionProblem.OriginatorSystemTitleMissing

        | Some title ->
            let bytes =
                title
                |> Ber.OctetString.value

            if bytes.Length = 8 then
                Ok bytes
            else
                Error (
                    UnprotectionProblem.InvalidOriginatorSystemTitleLength bytes.Length
                )

    let private retained envelope problem =
        {
            Envelope = envelope
            Problem = problem
        }

    let private expose
        (resolver: IGlobalCipherKeyResolver)
        (aarq: Field<AarqRaw>)
        (envelope: Field<GloInitiateRequestRaw>)
        (validation: Validation<GloInitiateRequestValidatedFields>)
        : Decoder<PayloadExposure> =

        match validation with
        | Failed (failures, _) ->
            UnprotectionProblem.InvalidProtectedApdu failures
            |> retained envelope
            |> ExposureNotAttempted.PayloadUnavailable
            |> Transformation.NotAttempted
            |> decodePassed

        | Passed (validated, _) ->
            match originatorSystemTitle aarq with
            | Error problem ->
                problem
                |> retained envelope
                |> ExposureNotAttempted.PayloadUnavailable
                |> Transformation.NotAttempted
                |> decodePassed

            | Ok systemTitle ->
                match resolver.ResolveGlobalKeys systemTitle with
                | None ->
                    UnprotectionProblem.GlobalKeysUnavailable
                    |> retained envelope
                    |> ExposureNotAttempted.PayloadUnavailable
                    |> Transformation.NotAttempted
                    |> decodePassed

                | Some keys ->
                    let cipherContext = {
                        OriginatorSystemTitle = systemTitle
                        GlobalUnicastEncryptionKey = keys.GlobalUnicastEncryptionKey
                        GlobalBroadcastEncryptionKey = keys.GlobalBroadcastEncryptionKey
                        AuthenticationKey = keys.AuthenticationKey
                    }

                    let protectedApdu =
                        GloInitiateRequestValidatedFields.protectedApdu validated

                    fun context ->
                        match GloCiphering.unprotect cipherContext protectedApdu context with
                        | Decoded (plain, notices) ->
                            match
                                createDerivedSource
                                    "Decrypted DLMS initiate-request"
                                    (SourceTransform.Decrypt "DLMS AES-GCM")
                                    true
                                    protectedApdu.Value.Payload
                                    plain.Value
                                    context
                            with
                            | Decoded (derived, sourceNotices) ->
                                Decoded (
                                    Transformation.Succeeded derived,
                                    notices @ sourceNotices
                                )

                            | DecodeFailed (failure, sourceNotices) ->
                                DecodeFailed (failure, notices @ sourceNotices)

                        | DecodeFailed (EncryptionFailed issue, notices) ->
                            UnprotectionProblem.CryptographicFailure issue
                            |> retained envelope
                            |> Transformation.Failed
                            |> fun result -> Decoded (result, notices)

                        | DecodeFailed (failure, notices) ->
                            DecodeFailed (failure, notices)

    let private blockedAfterWrapper wrapper =
        {
            Wrapper = wrapper
            Aarq = Stage.NotReached AarqNotReached.WrapperMalformed
            ProtectedApdu =
                Stage.NotReached ProtectedApduNotReached.WrapperMalformed
            Exposure =
                Transformation.NotAttempted ExposureNotAttempted.WrapperMalformed
            InitiateRequest =
                Stage.NotReached InitiateRequestNotReached.WrapperMalformed
        }

    let private blockedAfterAarq wrapper aarq =
        {
            Wrapper = wrapper
            Aarq = aarq
            ProtectedApdu =
                Stage.NotReached ProtectedApduNotReached.AarqMalformed
            Exposure =
                Transformation.NotAttempted ExposureNotAttempted.AarqMalformed
            InitiateRequest =
                Stage.NotReached InitiateRequestNotReached.AarqMalformed
        }

    let private blockedWithoutUserInformation wrapper aarq =
        {
            Wrapper = wrapper
            Aarq = aarq
            ProtectedApdu =
                Stage.NotReached ProtectedApduNotReached.UserInformationMissing
            Exposure =
                Transformation.NotAttempted ExposureNotAttempted.UserInformationMissing
            InitiateRequest =
                Stage.NotReached InitiateRequestNotReached.UserInformationMissing
        }

    /// DLMS Green Book Ed. 12:
    /// - 7.2.3.3.2: Wrapper data-length delimits the xDLMS/ACSE APDU;
    /// - 9.2.7.2: a protected APDU is restored before its service is invoked;
    /// - 9.3.2: calling-AP-title supplies the client system title in AARQ.
    let decode
        (resolver: IGlobalCipherKeyResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<TcpAarqDecodeReport> =

        decoder {
            let! wrapper =
                decodeStage WrapperPduRaw.parse WrapperPdu.fromRaw source

            match wrapper with
            | Stage.Malformed _ ->
                return blockedAfterWrapper wrapper

            | Stage.NotReached never ->
                return failwith $"Unreachable root-stage blocker: {never}"

            | Stage.Reached (wrapperRaw, _) ->
                let! aarq =
                    decodeStage
                        AarqRaw.parse
                        AarqValidatedFields.fromParsed
                        wrapperRaw.Value.Data

                match aarq with
                | Stage.Malformed _ ->
                    return blockedAfterAarq wrapper aarq

                | Stage.NotReached reason ->
                    return failwith $"Unreachable AARQ blocker: {reason}"

                | Stage.Reached (aarqRaw, _) ->
                    match userInformationSource aarqRaw with
                    | None ->
                        return blockedWithoutUserInformation wrapper aarq

                    | Some userInformation ->
                        let! protectedApdu =
                            decodeStage
                                GloInitiateRequestRaw.parse
                                GloInitiateRequestValidatedFields.fromRaw
                                userInformation

                        match protectedApdu with
                        | Stage.Malformed _ ->
                            return {
                                Wrapper = wrapper
                                Aarq = aarq
                                ProtectedApdu = protectedApdu
                                Exposure =
                                    Transformation.NotAttempted
                                        ExposureNotAttempted.ProtectedApduMalformed
                                InitiateRequest =
                                    Stage.NotReached
                                        InitiateRequestNotReached.ProtectedApduMalformed
                            }

                        | Stage.NotReached reason ->
                            return failwith $"Unreachable protected APDU blocker: {reason}"

                        | Stage.Reached (protectedRaw, validation) ->
                            let! exposure =
                                expose resolver aarqRaw protectedRaw validation

                            match exposure with
                            | Transformation.Succeeded plain ->
                                let! initiateRequest =
                                    decodeStage
                                        InitiateRequestRaw.parse
                                        InitiateRequestValidatedFields.fromParsed
                                        plain

                                return {
                                    Wrapper = wrapper
                                    Aarq = aarq
                                    ProtectedApdu = protectedApdu
                                    Exposure = exposure
                                    InitiateRequest = initiateRequest
                                }

                            | Transformation.Failed _
                            | Transformation.NotAttempted
                                (ExposureNotAttempted.PayloadUnavailable _) ->
                                return {
                                    Wrapper = wrapper
                                    Aarq = aarq
                                    ProtectedApdu = protectedApdu
                                    Exposure = exposure
                                    InitiateRequest =
                                        Stage.NotReached
                                            InitiateRequestNotReached.PayloadProtected
                                }

                            | Transformation.NotAttempted reason ->
                                return failwith $"Unreachable exposure blocker: {reason}"
        }
