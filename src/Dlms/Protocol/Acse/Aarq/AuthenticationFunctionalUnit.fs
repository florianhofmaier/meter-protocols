namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Fields

type AuthenticationFunctionalUnit =
    | NoSecurity
    | LowLevelSecurity of AuthenticationValue
    | HighLevelSecurity of HlsAuthenticationMechanismName * AuthenticationValue

module AuthenticationFunctionalUnit =
    let private validateMechanism
        (value: Field<MechanismName option>)
        : Validation<MechanismName> =
        requireSome
            "missing mechanism-name although authentication is selected"
            value

    let private validateValue
        (value: Field<AuthenticationValue option>)
        : Validation<AuthenticationValue> =
        requireSome
            "missing calling-authentication-value although authentication is selected"
            value

    let private buildSelectedAuthentication
        (mechanism: Field<MechanismName>)
        (value: Field<AuthenticationValue>)
        : Validation<Field<AuthenticationFunctionalUnit>> =

        match mechanism.Value with
        | MechanismName.LowestLevelSecurity ->
            failed
                mechanism
                "lowest-level-security is invalid when sender-acse-requirements selects authentication"

        | MechanismName.LowLevelSecurity ->
            passed (AuthenticationFunctionalUnit.LowLevelSecurity value.Value)

        | MechanismName.HighLevelSecurity hls ->
            passed (AuthenticationFunctionalUnit.HighLevelSecurity (hls, value.Value))

    let private validateSelected
        (mechanismName: Field<MechanismName option>)
        (callingAuthenticationValue: Field<AuthenticationValue option>)
        : Validation<Field<AuthenticationFunctionalUnit>> =
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