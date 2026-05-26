namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Fields

type AuthenticationFunctionalUnit =
    | NoSecurity
    | LowLevelSecurity of AuthenticationValue
    | HighLevelSecurity of HlsAuthenticationMechanismName * AuthenticationValue

module Authentication =
    let private validateMechanism
        (raw: Parsed<Ber.ObjectIdentifier option>)
        : Validation<MechanismName> =
        raw
        |> Validation.requireSome
            "missing mechanism-name although authentication is selected"
        |> Validation.bind MechanismName.validate

    let private validateValue
        (raw: AuthenticationValueRaw option)
        : Validation<AuthenticationValue> =
        raw
        |> Validation.requireSome
            "missing calling-authentication-value although authentication is selected"
        |> Validation.bind AuthenticationValue.fromParsed

    let private buildSelectedAuthentication mechanism value : Validation<AuthenticationFunctionalUnit> =
        match mechanism with
        | MechanismName.LowestLevelSecurity ->
            Validation.error
                "lowest-level-security is invalid when sender-acse-requirements selects authentication"

        | MechanismName.LowLevelSecurity ->
            Validation.ok (AuthenticationFunctionalUnit.LowLevelSecurity value)

        | MechanismName.HighLevelSecurity hls ->
            Validation.ok (AuthenticationFunctionalUnit.HighLevelSecurity (hls, value))

    let private validateSelected
        (mechanismName: Ber.ObjectIdentifier option)
        (callingAuthenticationValue: AuthenticationValueRaw option)
        : Validation<AuthenticationFunctionalUnit> =
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
        : Validation<AuthenticationFunctionalUnit> =

        validator {
            let! selection =
                SenderAcseRequirements.validate senderAcseRequirements

            match selection with
            | AuthenticationNotSelected ->
                return AuthenticationFunctionalUnit.NoSecurity

            | AuthenticationSelected ->
                return!
                    validateSelected
                        mechanismName
                        callingAuthenticationValue
        }