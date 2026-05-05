namespace DlmsMessages.Acse

open DlmsMessages
open Mbus.BaseParsers.Core

type AarqTag =
    | ProtocolVersion = 0x80uy
    | SenderAcseRequirements = 0x8Auy
    | MechanismName = 0x8Buy
    | ImplementationInformation = 0x9Duy
    | ApplicationContextName = 0xA1uy

    | CalledApTitle = 0xA2uy
    | CalledAeQualifier = 0xA3uy
    | CalledApInvocationId = 0xA4uy
    | CalledAeInvocationId = 0xA5uy

    | CallingApTitle = 0xA6uy
    | CallingAeQualifier = 0xA7uy
    | CallingApInvocationId = 0xA8uy
    | CallingAeInvocationId = 0xA9uy

    | CallingAuthenticationValue = 0xACuy
    | UserInformation = 0xBEuy


type AarqRawFields =
    {
        ProtocolVersion : Ber.BitString option
        ApplicationContextName : Ber.ObjectIdentifier option

        CalledApTitle : Ber.OctetString option
        CalledAeQualifier : Ber.OctetString option
        CalledApInvocationId : uint32 option
        CalledAeInvocationId : uint32 option

        CallingApTitle : Ber.OctetString option
        CallingAeQualifier : Ber.OctetString option
        CallingApInvocationId : uint32 option
        CallingAeInvocationId : uint32 option

        SenderAcseRequirements : Ber.BitString option
        MechanismName : Ber.ObjectIdentifier option
        ImplementationInformation : Ber.GraphicString option
        CallingAuthenticationValue : AuthenticationValueRaw option

        UserInformation : Ber.OctetString option
    }

module AarqRawFields =
    let parse : Parser<AarqRawFields> =
        Ber.parseLengthDelimited <|
            parser {
                let! protocolVersion =
                    withCtx "ProtocolVersion" <|
                        Ber.parseTaggedOptional
                            AarqTag.ProtocolVersion
                            Ber.BitString.parseContent

                let! applicationContextName =
                    withCtx "ApplicationContextName" <|
                        Ber.parseTaggedOptional
                            AarqTag.ApplicationContextName
                            Ber.ObjectIdentifier.parse

                let! calledApTitle =
                    withCtx "CalledApTitle" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApTitle
                            Ber.OctetString.parse

                let! calledAeQualifier =
                    withCtx "CalledAeQualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeQualifier
                            Ber.OctetString.parse

                let! calledApInvocationId =
                    withCtx "CalledApInvocationId" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApInvocationId
                            Ber.Integer.parseUint32

                let! calledAeInvocationId =
                    withCtx "CalledAeInvocationId" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeInvocationId
                            Ber.Integer.parseUint32

                let! callingApTitle =
                    withCtx "CallingApTitle" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApTitle
                            Ber.OctetString.parse

                let! callingAeQualifier =
                    withCtx "CallingAeQualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeQualifier
                            Ber.OctetString.parse

                let! callingApInvocationId =
                    withCtx "CallingApInvocationId" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApInvocationId
                            Ber.Integer.parseUint32

                let! callingAeInvocationId =
                    withCtx "CallingAeInvocationId" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeInvocationId
                            Ber.Integer.parseUint32

                let! senderAcseRequirements =
                    withCtx "SenderAcseRequirements" <|
                        Ber.parseTaggedOptional
                            AarqTag.SenderAcseRequirements
                            Ber.BitString.parseContent

                let! mechanismName =
                    withCtx "MechanismName" <|
                        Ber.parseTaggedOptional
                            AarqTag.MechanismName
                            Ber.ObjectIdentifier.parseContent

                let! callingAuthenticationValue =
                    withCtx "CallingAuthenticationValue" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAuthenticationValue
                            AuthenticationValueRaw.parse

                let! implementationInformation =
                    withCtx "ImplementationInformation" <|
                        Ber.parseTaggedOptional
                            AarqTag.ImplementationInformation
                            Ber.GraphicString.parseContent

                let! userInformation =
                    withCtx "UserInformation" <|
                        Ber.parseTaggedOptional
                            AarqTag.UserInformation
                            Ber.OctetString.parse

                return {
                    ProtocolVersion = protocolVersion
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
                    ImplementationInformation = implementationInformation

                    UserInformation = userInformation
                }
            }