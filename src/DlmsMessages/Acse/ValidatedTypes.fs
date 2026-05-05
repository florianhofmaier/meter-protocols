namespace DlmsMessages.Acse

open System
open DlmsMessages
open Validators

type ProtocolVersion =
    | Version1

module ProtocolVersion =

    let private isVersion1 (bits: Ber.BitString) =
        bits.UnusedBitCount = 7uy
        && bits.Payload.Length = 1
        && bits.Payload.Span[0] = 0x80uy

    let validate (raw: Ber.BitString option) : Validation<Unit> =
        match raw with
        | None ->
            Validation.ok ()

        | Some bits when isVersion1 bits ->
            Validation.ok ()

        | Some _ ->
            Validation.error
                ["AARQ"; "protocol-version"]
                "unsupported protocol-version value"

module private OidValidation =
    let hasPrefixAndSingleId (prefix: byte[]) (value: ReadOnlyMemory<byte>) =
        let span = value.Span

        span.Length = prefix.Length + 1
        && span.Slice(0, prefix.Length).SequenceEqual(prefix.AsSpan())

    let lastByte (value: ReadOnlyMemory<byte>) =
        value.Span[value.Length - 1]

type ApplicationContextName =
    | LogicalNameNoCiphering
    | ShortNameNoCiphering
    | LogicalNameWithCiphering
    | ShortNameWithCiphering

module ApplicationContextName =
    let validate oid : Validation<ApplicationContextName> =
        let bytes = Ber.ObjectIdentifier.toBytes oid
        let prefix = [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x01uy |]

        if not (OidValidation.hasPrefixAndSingleId prefix bytes) then
            Validation.error
                ["AARQ"; "application-context-name"]
                "invalid application-context-name object identifier"
        else
            match OidValidation.lastByte bytes with
            | 0x01uy -> Validation.ok LogicalNameNoCiphering
            | 0x02uy -> Validation.ok ShortNameNoCiphering
            | 0x03uy -> Validation.ok LogicalNameWithCiphering
            | 0x04uy -> Validation.ok ShortNameWithCiphering
            | value ->
                Validation.error
                    ["AARQ"; "application-context-name"]
                    $"unsupported application-context-name id 0x{value:X2}"

type ApTitle =
    private
        ApTitle of Ber.OctetString

module ApTitle =
    let create value = ApTitle value
    let value (ApTitle value) = value

type AeQualifier =
    private AeQualifier of Ber.OctetString

module AeQualifier =
    let create value = AeQualifier value
    let value (AeQualifier value) = value

type InvocationIdentifier =
    private InvocationIdentifier of uint32

module InvocationIdentifier =
    let create value = InvocationIdentifier value
    let value (InvocationIdentifier value) = value

type UserInformation =
    private UserInformation of Ber.OctetString

module UserInformation =
    let create value = UserInformation value

    let value (UserInformation value) = value

    let validate raw : Validation<UserInformation> =
        raw
        |> Validation.requireSome
            ["AARQ"; "user-information"]
            "missing user-information"
        |> Validation.map create

type AuthenticationValue =
    private AuthenticationValue of ReadOnlyMemory<byte>

module AuthenticationValue =
    let value (AuthenticationValue value) = value

    let validate raw : Validation<AuthenticationValue> =
        match raw with
        | AuthenticationValueRaw.CharString value ->
            value
            |> Ber.GraphicString.toBytes
            |> AuthenticationValue
            |> Validation.ok

        | AuthenticationValueRaw.BitString _ ->
            Validation.error
                ["AARQ"; "calling-authentication-value"]
                "BIT STRING authentication-value is valid ASN.1 but not supported by this library"

type HlsAuthenticationMechanismName =
    | HighLevelSecurityManufacturerSpecific
    | HighLevelSecurityMd5
    | HighLevelSecuritySha1
    | HighLevelSecurityGmac
    | HighLevelSecuritySha256
    | HighLevelSecurityEcdsa

type AuthenticationMechanism =
    | LowestLevelSecurity
    | LowLevelSecurity
    | HighLevelSecurity of HlsAuthenticationMechanismName

module AuthenticationMechanism =
    let validate
        (oid: Ber.ObjectIdentifier)
        : Validation<AuthenticationMechanism> =

        let bytes = Ber.ObjectIdentifier.toBytes oid
        let prefix = [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x02uy |]

        if not (OidValidation.hasPrefixAndSingleId prefix bytes) then
            Validation.error
                ["AARQ"; "mechanism-name"]
                "invalid authentication mechanism object identifier"
        else
            match OidValidation.lastByte bytes with
            | 0x00uy -> Validation.ok LowestLevelSecurity
            | 0x01uy -> Validation.ok LowLevelSecurity
            | 0x02uy -> Validation.ok (HighLevelSecurity HighLevelSecurityManufacturerSpecific)
            | 0x03uy -> Validation.ok (HighLevelSecurity HighLevelSecurityMd5)
            | 0x04uy -> Validation.ok (HighLevelSecurity HighLevelSecuritySha1)
            | 0x05uy -> Validation.ok (HighLevelSecurity HighLevelSecurityGmac)
            | 0x06uy -> Validation.ok (HighLevelSecurity HighLevelSecuritySha256)
            | 0x07uy -> Validation.ok (HighLevelSecurity HighLevelSecurityEcdsa)
            | value ->
                Validation.error
                    ["AARQ"; "mechanism-name"]
                    $"unsupported authentication mechanism id 0x{value:X2}"

type SenderAcseRequirements =
    | AuthenticationNotSelected
    | AuthenticationSelected

module SenderAcseRequirements =
    let private isAuthenticationBitSet (bits: Ber.BitString) =
        bits.Payload.Length > 0
        && (bits.Payload.Span[0] &&& 0x80uy) <> 0uy

    let validate
        (raw: Ber.BitString option)
        : Validation<SenderAcseRequirements> =

        match raw with
        | Some bits when isAuthenticationBitSet bits ->
            Validation.ok AuthenticationSelected

        | _ ->
            Validation.ok AuthenticationNotSelected

type Authentication =
    | NoSecurity
    | LowLevelSecurity of AuthenticationValue
    | HighLevelSecurity of HlsAuthenticationMechanismName * AuthenticationValue

module Authentication =
    let private validateMechanism
        (raw: Ber.ObjectIdentifier option)
        : Validation<AuthenticationMechanism> =
        raw
        |> Validation.requireSome
            ["AARQ"; "mechanism-name"]
            "missing mechanism-name although authentication is selected"
        |> Validation.bind AuthenticationMechanism.validate

    let private validateValue
        (raw: AuthenticationValueRaw option)
        : Validation<AuthenticationValue> =
        raw
        |> Validation.requireSome
            ["AARQ"; "calling-authentication-value"]
            "missing calling-authentication-value although authentication is selected"
        |> Validation.bind AuthenticationValue.validate

    let private buildSelectedAuthentication mechanism value : Validation<Authentication> =
        match mechanism with
        | AuthenticationMechanism.LowestLevelSecurity ->
            Validation.error
                ["AARQ"; "mechanism-name"]
                "lowest-level-security is invalid when sender-acse-requirements selects authentication"

        | AuthenticationMechanism.LowLevelSecurity ->
            Validation.ok (Authentication.LowLevelSecurity value)

        | AuthenticationMechanism.HighLevelSecurity hls ->
            Validation.ok (Authentication.HighLevelSecurity (hls, value))

    let private validateSelected
        (mechanismName: Ber.ObjectIdentifier option)
        (callingAuthenticationValue: AuthenticationValueRaw option)
        : Validation<Authentication> =
        validator {
            let! mechanism =
                validateMechanism mechanismName

            and! value =
                validateValue callingAuthenticationValue

            return!
                buildSelectedAuthentication mechanism value
        }

    let validate
        (senderAcseRequirements: Ber.BitString option)
        (mechanismName: Ber.ObjectIdentifier option)
        (callingAuthenticationValue: AuthenticationValueRaw option)
        : Validation<Authentication> =

        validator {
            let! selection =
                SenderAcseRequirements.validate senderAcseRequirements

            match selection with
            | AuthenticationNotSelected ->
                return Authentication.NoSecurity

            | AuthenticationSelected ->
                return!
                    validateSelected
                        mechanismName
                        callingAuthenticationValue
        }

type ImplementationInformation =
    private ImplementationInformation of Ber.GraphicString

module ImplementationInformation =
    let create value = ImplementationInformation value
    let value (ImplementationInformation value) = value

type AssociationResult =
    | Accepted
    | RejectedPermanent
    | RejectedTransient

module AssociationResult =
    let fromRaw (raw: uint32) : Validation<AssociationResult> =
        match raw with
        | 0u -> Validation.ok Accepted
        | 1u -> Validation.ok RejectedPermanent
        | 2u -> Validation.ok RejectedTransient
        | value ->
            Validation.error
                ["AARE"; "result"]
                $"unsupported association-result value {value}"

type AcseServiceUserDiagnostic =
    | Null
    | NoReasonGiven
    | ApplicationContextNameNotSupported
    | CallingApTitleNotRecognized
    | CallingApInvocationIdentifierNotRecognized
    | CallingAeQualifierNotRecognized
    | CallingAeInvocationIdentifierNotRecognized
    | CalledApTitleNotRecognized
    | CalledApInvocationIdentifierNotRecognized
    | CalledAeQualifierNotRecognized
    | CalledAeInvocationIdentifierNotRecognized
    | AuthenticationMechanismNameNotRecognized
    | AuthenticationMechanismNameRequired
    | AuthenticationFailure
    | AuthenticationRequired

module AcseServiceUserDiagnostic =
    let fromRaw (raw: uint32) : Validation<AcseServiceUserDiagnostic> =
        match raw with
        | 0u -> Validation.ok Null
        | 1u -> Validation.ok NoReasonGiven
        | 2u -> Validation.ok ApplicationContextNameNotSupported
        | 3u -> Validation.ok CallingApTitleNotRecognized
        | 4u -> Validation.ok CallingApInvocationIdentifierNotRecognized
        | 5u -> Validation.ok CallingAeQualifierNotRecognized
        | 6u -> Validation.ok CallingAeInvocationIdentifierNotRecognized
        | 7u -> Validation.ok CalledApTitleNotRecognized
        | 8u -> Validation.ok CalledApInvocationIdentifierNotRecognized
        | 9u -> Validation.ok CalledAeQualifierNotRecognized
        | 10u -> Validation.ok CalledAeInvocationIdentifierNotRecognized
        | 11u -> Validation.ok AuthenticationMechanismNameNotRecognized
        | 12u -> Validation.ok AuthenticationMechanismNameRequired
        | 13u -> Validation.ok AuthenticationFailure
        | 14u -> Validation.ok AuthenticationRequired
        | value ->
            Validation.error
                ["AARE"; "result-source-diagnostic"; "acse-service-user"]
                $"unsupported acse-service-user diagnostic value {value}"

type AcseServiceProviderDiagnostic =
    | Null
    | NoReasonGiven
    | NoCommonAcseVersion

module AcseServiceProviderDiagnostic =
    let fromRaw (raw: uint32) : Validation<AcseServiceProviderDiagnostic> =
        match raw with
        | 0u -> Validation.ok Null
        | 1u -> Validation.ok NoReasonGiven
        | 2u -> Validation.ok NoCommonAcseVersion
        | value ->
            Validation.error
                ["AARE"; "result-source-diagnostic"; "acse-service-provider"]
                $"unsupported acse-service-provider diagnostic value {value}"

type AssociateSourceDiagnostic =
    | AcseServiceUser of AcseServiceUserDiagnostic
    | AcseServiceProvider of AcseServiceProviderDiagnostic

module AssociateSourceDiagnostic =
    let fromRaw raw : Validation<AssociateSourceDiagnostic> =
        match raw with
        | AssociateSourceDiagnosticRaw.AcseServiceUser value ->
            value
            |> AcseServiceUserDiagnostic.fromRaw
            |> Validation.map AssociateSourceDiagnostic.AcseServiceUser

        | AssociateSourceDiagnosticRaw.AcseServiceProvider value ->
            value
            |> AcseServiceProviderDiagnostic.fromRaw
            |> Validation.map AssociateSourceDiagnostic.AcseServiceProvider

type ResponderAcseRequirements =
    | AuthenticationNotSelected
    | AuthenticationSelected

module ResponderAcseRequirements =
    let private isAuthenticationBitSet (bits: Ber.BitString) =
        bits.Payload.Length > 0
        && (bits.Payload.Span[0] &&& 0x80uy) <> 0uy

    let fromRaw raw : Validation<ResponderAcseRequirements> =
        match raw with
        | Some bits when isAuthenticationBitSet bits ->
            Validation.ok AuthenticationSelected

        | None
        | Some _ ->
            Validation.ok AuthenticationNotSelected

type RespondingAuthentication =
    | NoAuthenticationFunctionalUnit
    | AuthenticationFunctionalUnitSelected of
        mechanism : AuthenticationMechanism option *
        value : AuthenticationValue option

module RespondingAuthentication =
    let private mechanismFromRaw raw : Validation<AuthenticationMechanism option> =
        raw
        |> Validation.traverseOption AuthenticationMechanism.validate

    let private valueFromRaw
        (raw: AuthenticationValueRaw option)
        : Validation<AuthenticationValue option> =

        raw
        |> Validation.traverseOption AuthenticationValue.validate

    let fromRaw
        (responderAcseRequirements: Ber.BitString option)
        (mechanismName: Ber.ObjectIdentifier option)
        (respondingAuthenticationValue: AuthenticationValueRaw option)
        : Validation<RespondingAuthentication> =

        validator {
            let! requirements =
                ResponderAcseRequirements.fromRaw responderAcseRequirements

            match requirements with
            | ResponderAcseRequirements.AuthenticationNotSelected ->
                return NoAuthenticationFunctionalUnit

            | ResponderAcseRequirements.AuthenticationSelected ->
                let! mechanism =
                    mechanismFromRaw mechanismName

                and! value =
                    valueFromRaw respondingAuthenticationValue

                return
                    AuthenticationFunctionalUnitSelected(
                        mechanism = mechanism,
                        value = value)
        }