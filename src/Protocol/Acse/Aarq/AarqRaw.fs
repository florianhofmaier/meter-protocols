namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Parsers.Core
open Metering.Dlms.Protocol.Acse
open Metering.Dlms.Protocol.Acse.Fields
open Metering.Common.Parsers.ParserTree
open Metering.Dlms.Protocol

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

type AarqRaw =
    {
        ProtocolVersion : Parsed<Ber.BitString option>
        ApplicationContextName : Parsed<Ber.ObjectIdentifier option>

        CalledApTitle : Parsed<Ber.OctetString option>
        CalledAeQualifier : Parsed<Ber.OctetString option>
        CalledApInvocationId : Parsed<uint32 option>
        CalledAeInvocationId : Parsed<uint32 option>

        CallingApTitle : Parsed<Ber.OctetString option>
        CallingAeQualifier : Parsed<Ber.OctetString option>
        CallingApInvocationId : Parsed<uint32 option>
        CallingAeInvocationId : Parsed<uint32 option>

        SenderAcseRequirements : Parsed<Ber.BitString option>
        MechanismName : Parsed<Ber.ObjectIdentifier option>
        ImplementationInformation : Parsed<Ber.GraphicString option>
        CallingAuthenticationValue : Parsed<AuthenticationValueRaw option>

        UserInformation : Parsed<Ber.OctetString option>
    }

module AarqRaw =

    let parseBody : Parser<AarqRaw> =
        Ber.parseLengthDelimited <|
            parser {
                let! protocolVersion =
                    parseNode "protocol-version" <|
                        Ber.parseTaggedOptional
                            AarqTag.ProtocolVersion
                            Ber.BitString.parseContent

                let! applicationContextName =
                    parseNode "application-context-name" <|
                        Ber.parseTaggedOptional
                            AarqTag.ApplicationContextName
                            Ber.ObjectIdentifier.parse

                let! calledApTitle =
                    parseNode "called-ap-title" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApTitle
                            Ber.OctetString.parse

                let! calledAeQualifier =
                    parseNode "called-ae-qualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeQualifier
                            Ber.OctetString.parse

                let! calledApInvocationId =
                    parseNode "called-ap-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApInvocationId
                            Ber.Integer.parseUint32

                let! calledAeInvocationId =
                    parseNode "called-ae-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeInvocationId
                            Ber.Integer.parseUint32

                let! callingApTitle =
                    parseNode "calling-ap-title" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApTitle
                            Ber.OctetString.parse

                let! callingAeQualifier =
                    parseNode "calling-ae-qualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeQualifier
                            Ber.OctetString.parse

                let! callingApInvocationId =
                    parseNode "calling-ap-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApInvocationId
                            Ber.Integer.parseUint32

                let! callingAeInvocationId =
                    parseNode "calling-ae-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeInvocationId
                            Ber.Integer.parseUint32

                let! senderAcseRequirements =
                    parseNode "sender-acse-requirements" <|
                        Ber.parseTaggedOptional
                            AarqTag.SenderAcseRequirements
                            Ber.BitString.parseContent

                let! mechanismName =
                    parseNode "mechanism-name" <|
                        Ber.parseTaggedOptional
                            AarqTag.MechanismName
                            Ber.ObjectIdentifier.parseContent

                let! callingAuthenticationValue =
                    parseNode "calling-authentication-value" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAuthenticationValue
                            AuthenticationValueRaw.parse

                let! implementationInformation =
                    parseNode "implementation-information" <|
                        Ber.parseTaggedOptional
                            AarqTag.ImplementationInformation
                            Ber.GraphicString.parseContent

                let! userInformation =
                    parseNode "user-information" <|
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

    let parse : Parser<Parsed<AarqRaw>> =
        parseNode "AARQ" <|
            parser {
                do! Tag.expect<AcseTag> AcseTag.Aarq
                return! parseBody
            }