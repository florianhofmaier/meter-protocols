namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
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

        UserInformation : Field<UserInformation>
    }

module AarqValidatedFields =

    let fromParsed
        (raw: Field<AarqRaw>)
        : Validation<AarqValidatedFields> =

        validator {
            let! _ =
                AcseField.defaulted
                    ProtocolVersion.Version1
                    (InfoWhenPresent "protocol-version is present although the default value would be sufficient")
                    ProtocolVersion.validateValue
                    raw.Value.ProtocolVersion

            and! applicationContextName =
                AcseField.required
                    "missing application-context-name"
                    ApplicationContextName.validate
                    raw.Value.ApplicationContextName

            and! calledApTitle =
                AcseField.optional
                    NoPresenceDiagnostic
                    ApTitle.validate
                    raw.Value.CalledApTitle

            and! calledAeQualifier =
                AcseField.optional
                    NoPresenceDiagnostic
                    AeQualifier.validate
                    raw.Value.CalledAeQualifier

            and! calledApInvocationId =
                AcseField.optional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CalledApInvocationId

            and! calledAeInvocationId =
                AcseField.optional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CalledApInvocationId

            and! callingApTitle =
                AcseField.optional
                    NoPresenceDiagnostic
                    ApTitle.validate
                    raw.Value.CallingApTitle

            and! callingAeQualifier =
                AcseField.optional
                    NoPresenceDiagnostic
                    AeQualifier.validate
                    raw.Value.CallingAeQualifier

            and! callingApInvocationId =
                AcseField.optional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CallingApInvocationId

            and! callingAeInvocationId =
                AcseField.optional
                    NoPresenceDiagnostic
                    InvocationIdentifier.validate
                    raw.Value.CallingAeInvocationId

            and! senderAcseRequirements =
                AcseField.optional
                    NoPresenceDiagnostic
                    SenderAcseRequirements.validate
                    raw.Value.SenderAcseRequirements

            and! mechanismName =
                AcseField.optional
                    NoPresenceDiagnostic
                    MechanismName.validate
                    raw.Value.MechanismName

            and! callingAuthenticationValue =
                AcseField.optional
                    NoPresenceDiagnostic
                    AuthenticationValue.validate
                    raw.Value.CallingAuthenticationValue

            and! _ =
                AcseField.optional
                    (InfoWhenPresent "implementation-information is not used by DLMS/COSEM AL")
                    ImplementationInformation.validate
                    raw.Value.ImplementationInformation

            and! userInformation =
                AcseField.requiredField
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