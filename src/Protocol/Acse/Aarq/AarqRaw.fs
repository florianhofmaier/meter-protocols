namespace Metering.Dlms.Protocol.Acse.Aarq

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse
open Metering.Dlms.Protocol.Acse.Fields


type AarqRaw =
    {
        ProtocolVersion : ParsedField<Ber.BitString option>
        ApplicationContextName : ParsedField<Ber.ObjectIdentifier option>

        CalledApTitle : ParsedField<Ber.OctetString option>
        CalledAeQualifier : ParsedField<Ber.OctetString option>
        CalledApInvocationId : ParsedField<uint32 option>
        CalledAeInvocationId : ParsedField<uint32 option>

        CallingApTitle : ParsedField<Ber.OctetString option>
        CallingAeQualifier : ParsedField<Ber.OctetString option>
        CallingApInvocationId : ParsedField<uint32 option>
        CallingAeInvocationId : ParsedField<uint32 option>

        SenderAcseRequirements : ParsedField<Ber.BitString option>
        MechanismName : ParsedField<Ber.ObjectIdentifier option>
        ImplementationInformation : ParsedField<Ber.GraphicString option>
        CallingAuthenticationValue : ParsedField<AuthenticationValueRaw option>

        UserInformation : ParsedField<Ber.OctetString option>
    }

module AarqRaw =

    let parseBody : Parser<AarqRaw> =
        Ber.parseLengthDelimited <|
            parser {
                let! protocolVersion =
                    parseField "protocol-version" <|
                        Ber.parseTaggedOptional
                            AarqTag.ProtocolVersion
                            Ber.BitString.parseContent

                let! applicationContextName =
                    parseField "application-context-name" <|
                        Ber.parseTaggedOptional
                            AarqTag.ApplicationContextName
                            Ber.ObjectIdentifier.parse

                let! calledApTitle =
                    parseField "called-ap-title" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApTitle
                            Ber.OctetString.parse

                let! calledAeQualifier =
                    parseField "called-ae-qualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeQualifier
                            Ber.OctetString.parse

                let! calledApInvocationId =
                    parseField "called-ap-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledApInvocationId
                            Ber.Integer.parseUint32

                let! calledAeInvocationId =
                    parseField "called-ae-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CalledAeInvocationId
                            Ber.Integer.parseUint32

                let! callingApTitle =
                    parseField "calling-ap-title" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApTitle
                            Ber.OctetString.parse

                let! callingAeQualifier =
                    parseField "calling-ae-qualifier" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeQualifier
                            Ber.OctetString.parse

                let! callingApInvocationId =
                    parseField "calling-ap-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingApInvocationId
                            Ber.Integer.parseUint32

                let! callingAeInvocationId =
                    parseField "calling-ae-invocation-id" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAeInvocationId
                            Ber.Integer.parseUint32

                let! senderAcseRequirements =
                    parseField "sender-acse-requirements" <|
                        Ber.parseTaggedOptional
                            AarqTag.SenderAcseRequirements
                            Ber.BitString.parseContent

                let! mechanismName =
                    parseField "mechanism-name" <|
                        Ber.parseTaggedOptional
                            AarqTag.MechanismName
                            Ber.ObjectIdentifier.parseContent

                let! callingAuthenticationValue =
                    parseField "calling-authentication-value" <|
                        Ber.parseTaggedOptional
                            AarqTag.CallingAuthenticationValue
                            AuthenticationValueRaw.parse

                let! implementationInformation =
                    parseField "implementation-information" <|
                        Ber.parseTaggedOptional
                            AarqTag.ImplementationInformation
                            Ber.GraphicString.parseContent

                let! userInformation =
                    parseField "user-information" <|
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

    let parse : Parser<ParsedField<AarqRaw>> =
        parseField "AARQ" <|
            parser {
                do! Tag.expect<AcseTag> AcseTag.Aarq
                return! parseBody
            }