namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators
open Metering.Common.Validators.Core
open Metering.Common.Validators.Fields
open Metering.Dlms.Protocol.Acse.Fields

type AarqValidatedFields =
    {
        ApplicationContextName : ApplicationContextName

        CalledApTitle : ApTitle option
        CalledAeQualifier : AeQualifier option
        CalledApInvocationId : InvocationIdentifier option
        CalledAeInvocationId : InvocationIdentifier option

        CallingApTitle : ApTitle option
        CallingAeQualifier : AeQualifier option
        CallingApInvocationId : InvocationIdentifier option
        CallingAeInvocationId : InvocationIdentifier option

        SenderAcseRequirements : SenderAcseRequirements option
        MechanismName : MechanismName option
        CallingAuthenticationValue : AuthenticationValue option

        UserInformation : Parsed<UserInformation>
    }

module AarqValidatedFields =

    let fromParsed
        (raw: Parsed<AarqRaw>)
        : Validation<AarqValidatedFields> =

        validator {
            let! _ =
                FieldValidation.fromParsedDefaulted
                    ProtocolVersion.Version1
                    (InfoWhenPresent "protocol-version is present although the default value would be sufficient")
                    ProtocolVersion.validateValue
                    raw.Value.ProtocolVersion

            and! applicationContextName =
                FieldValidation.fromParsedRequired
                    "missing application-context-name"
                    ApplicationContextName.validate
                    raw.Value.ApplicationContextName

            and! calledApTitle =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    ApTitle.validate
                    raw.Value.CalledApTitle

            and! calledAeQualifier =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    AeQualifier.validate
                    raw.Value.CalledAeQualifier

            and! calledApInvocationId =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CalledApInvocationId

            and! calledAeInvocationId =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CalledApInvocationId

            and! callingApTitle =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    ApTitle.validate
                    raw.Value.CallingApTitle

            and! callingAeQualifier =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    AeQualifier.validate
                    raw.Value.CallingAeQualifier

            and! callingApInvocationId =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CallingApInvocationId

            and! callingAeInvocationId =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CallingAeInvocationId

            and! senderAcseRequirements =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    SenderAcseRequirements.validate
                    raw.Value.SenderAcseRequirements

            and! mechanismName =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    MechanismName.validate
                    raw.Value.MechanismName

            and! callingAuthenticationValue =
                FieldValidation.fromParsedOptional
                    NoPresenceDiagnostic
                    AuthenticationValue.validate
                    raw.Value.CallingAuthenticationValue

            and! _ =
                FieldValidation.fromParsedOptional
                    (InfoWhenPresent "implementation-information is not used by DLMS/COSEM AL")
                    ImplementationInformation.validate
                    raw.Value.ImplementationInformation

            and! userInformation =
                FieldValidation.fromParsedRequiredKeepingSource
                    "missing user-information"
                    UserInformation.validate raw.Value.UserInformation

            return {
                ApplicationContextName = applicationContextName

                CalledApTitle = calledApTitle
                CalledAeQualifier = calledAeQualifier
                CalledApInvocationId = calledApInvocationId
                CalledAeInvocationId = calledAeInvocationId

                CallingApTitle = callingApTitle
                CallingAeQualifier = callingAeQualifier
                CallingApInvocationId = callingApInvocationId
                CallingAeInvocationId = callingAeInvocationId

                SenderAcseRequirements = senderAcseRequirements
                MechanismName = mechanismName
                CallingAuthenticationValue = callingAuthenticationValue

                UserInformation = userInformation
            }
        }