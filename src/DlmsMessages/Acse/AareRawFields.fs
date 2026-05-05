namespace DlmsMessages.Acse

open DlmsMessages
open Mbus.BaseParsers.Core

type AareTag =
    | ProtocolVersion = 0x80uy
    | ApplicationContextName = 0xA1uy
    | Result = 0xA2uy
    | ResultSourceDiagnostic = 0xA3uy

    | RespondingApTitle = 0xA4uy
    | RespondingAeQualifier = 0xA5uy
    | RespondingApInvocationId = 0xA6uy
    | RespondingAeInvocationId = 0xA7uy

    | ResponderAcseRequirements = 0x88uy
    | MechanismName = 0x89uy
    | RespondingAuthenticationValue = 0xAAuy

    | ImplementationInformation = 0x9Duy
    | UserInformation = 0xBEuy

type AareRawFields =
    {
        ProtocolVersion : Ber.BitString option
        ApplicationContextName : Ber.ObjectIdentifier option

        Result : uint32 option
        ResultSourceDiagnostic : AssociateSourceDiagnosticRaw option

        RespondingApTitle : Ber.OctetString option
        RespondingAeQualifier : Ber.OctetString option
        RespondingApInvocationId : uint32 option
        RespondingAeInvocationId : uint32 option

        ResponderAcseRequirements : Ber.BitString option
        MechanismName : Ber.ObjectIdentifier option
        RespondingAuthenticationValue : AuthenticationValueRaw option

        ImplementationInformation : Ber.GraphicString option
        UserInformation : Ber.OctetString option
    }

module AareRawFields =
    let parse : Parser<AareRawFields> =
        Ber.parseLengthDelimited <|
            parser {
                let! protocolVersion =
                    withCtx "ProtocolVersion" <|
                        Ber.parseTaggedOptional
                            AareTag.ProtocolVersion
                            Ber.BitString.parseContent

                let! applicationContextName =
                    withCtx "ApplicationContextName" <|
                        Ber.parseTaggedOptional
                            AareTag.ApplicationContextName
                            Ber.ObjectIdentifier.parse

                let! result =
                    withCtx "Result" <|
                        Ber.parseTaggedOptional
                            AareTag.Result
                            Ber.Integer.parseUint32

                let! resultSourceDiagnostic =
                    withCtx "ResultSourceDiagnostic" <|
                        Ber.parseTaggedOptional
                            AareTag.ResultSourceDiagnostic
                            AssociateSourceDiagnosticRaw.parse

                let! respondingApTitle =
                    withCtx "RespondingApTitle" <|
                        Ber.parseTaggedOptional
                            AareTag.RespondingApTitle
                            Ber.OctetString.parse

                let! respondingAeQualifier =
                    withCtx "RespondingAeQualifier" <|
                        Ber.parseTaggedOptional
                            AareTag.RespondingAeQualifier
                            Ber.OctetString.parse

                let! respondingApInvocationId =
                    withCtx "RespondingApInvocationId" <|
                        Ber.parseTaggedOptional
                            AareTag.RespondingApInvocationId
                            Ber.Integer.parseUint32

                let! respondingAeInvocationId =
                    withCtx "RespondingAeInvocationId" <|
                        Ber.parseTaggedOptional
                            AareTag.RespondingAeInvocationId
                            Ber.Integer.parseUint32

                let! responderAcseRequirements =
                    withCtx "ResponderAcseRequirements" <|
                        Ber.parseTaggedOptional
                            AareTag.ResponderAcseRequirements
                            Ber.BitString.parseContent

                let! mechanismName =
                    withCtx "MechanismName" <|
                        Ber.parseTaggedOptional
                            AareTag.MechanismName
                            Ber.ObjectIdentifier.parseContent

                let! respondingAuthenticationValue =
                    withCtx "RespondingAuthenticationValue" <|
                        Ber.parseTaggedOptional
                            AareTag.RespondingAuthenticationValue
                            AuthenticationValueRaw.parse

                let! implementationInformation =
                    withCtx "ImplementationInformation" <|
                        Ber.parseTaggedOptional
                            AareTag.ImplementationInformation
                            Ber.GraphicString.parseContent

                let! userInformation =
                    withCtx "UserInformation" <|
                        Ber.parseTaggedOptional
                            AareTag.UserInformation
                            Ber.OctetString.parse

                return {
                    ProtocolVersion = protocolVersion
                    ApplicationContextName = applicationContextName

                    Result = result
                    ResultSourceDiagnostic = resultSourceDiagnostic

                    RespondingApTitle = respondingApTitle
                    RespondingAeQualifier = respondingAeQualifier
                    RespondingApInvocationId = respondingApInvocationId
                    RespondingAeInvocationId = respondingAeInvocationId

                    ResponderAcseRequirements = responderAcseRequirements
                    MechanismName = mechanismName
                    RespondingAuthenticationValue = respondingAuthenticationValue

                    ImplementationInformation = implementationInformation
                    UserInformation = userInformation
                }
            }