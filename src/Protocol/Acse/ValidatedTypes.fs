namespace Metering.Dlms.Protocol.Acse

open Metering.Dlms.Protocol



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