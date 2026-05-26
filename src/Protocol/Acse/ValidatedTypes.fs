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
            validationError
                ["AARQ"; "mechanism-name"]
                "lowest-level-security is invalid when sender-acse-requirements selects authentication"

        | AuthenticationMechanism.LowLevelSecurity ->
            validationOk(Authentication.LowLevelSecurity value)

        | AuthenticationMechanism.HighLevelSecurity hls ->
            validationOk(Authentication.HighLevelSecurity (hls, value))

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
        | 0u ->validationOk Accepted
        | 1u ->validationOk RejectedPermanent
        | 2u ->validationOk RejectedTransient
        | value ->
            validationError
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
        | 0u ->validationOk Null
        | 1u ->validationOk NoReasonGiven
        | 2u ->validationOk ApplicationContextNameNotSupported
        | 3u ->validationOk CallingApTitleNotRecognized
        | 4u ->validationOk CallingApInvocationIdentifierNotRecognized
        | 5u ->validationOk CallingAeQualifierNotRecognized
        | 6u ->validationOk CallingAeInvocationIdentifierNotRecognized
        | 7u ->validationOk CalledApTitleNotRecognized
        | 8u ->validationOk CalledApInvocationIdentifierNotRecognized
        | 9u ->validationOk CalledAeQualifierNotRecognized
        | 10u ->validationOk CalledAeInvocationIdentifierNotRecognized
        | 11u ->validationOk AuthenticationMechanismNameNotRecognized
        | 12u ->validationOk AuthenticationMechanismNameRequired
        | 13u ->validationOk AuthenticationFailure
        | 14u ->validationOk AuthenticationRequired
        | value ->
            validationError
                ["AARE"; "result-source-diagnostic"; "acse-service-user"]
                $"unsupported acse-service-user diagnostic value {value}"

type AcseServiceProviderDiagnostic =
    | Null
    | NoReasonGiven
    | NoCommonAcseVersion

module AcseServiceProviderDiagnostic =
    let fromRaw (raw: uint32) : Validation<AcseServiceProviderDiagnostic> =
        match raw with
        | 0u ->validationOk Null
        | 1u ->validationOk NoReasonGiven
        | 2u ->validationOk NoCommonAcseVersion
        | value ->
            validationError
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
            validationOkAuthenticationSelected

        | None
        | Some _ ->
            validationOkAuthenticationNotSelected

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