namespace DlmsMessages
//
// open System
// open DlmsMessages.Xdlms
// open Mbus.BaseParsers.BinaryParsers
// open Mbus.BaseParsers.Core
// open Mbus.BaseParsers.Utility
//
// type XdlmsApdu =
//     | InitiateRequest of InitiateRequest
//     | InitiateResponse of InitiateResponse
//     | ReadRequest of ReadRequest
//     | ReadResponse of ReadResponse
//     | WriteRequest of WriteRequest
//     | WriteResponse of WriteResponse
//     | GetRequest of GetRequest
//     | GetResponse of GetResponse
//     | SetRequest of SetRequest
//     | SetResponse of SetResponse
//     | ActionRequest of ActionRequest
//     | ActionResponse of ActionResponse
//     | AccessRequest of AccessRequest
//     | AccessResponse of AccessResponse
//     | DataNotification of DataNotification
//     | DataNotificationConfirm of DataNotificationConfirm
//     | InformationReportRequest of InformationReportRequest
//     | UnconfirmedWriteRequest of UnconfirmedWriteRequest
//     | ConfirmedServiceError of ConfirmedServiceError
//     | ExceptionResponse of ExceptionResponse
//     | GeneralGloCiphering of GeneralGloCiphering
//     | GeneralDedCiphering of GeneralDedCiphering
//     | GeneralCiphering of GeneralCiphering
//     | GeneralSigning of GeneralSigning
//     | GeneralBlockTransfer of GeneralBlockTransfer
//
//
// type DlmsVersionNumber = DlmsVersionNumber of byte
// type ClientMaxReceivePduSize = ClientMaxReceivePduSize of uint16
//
// type ClientIdentity =
//     | Anonymous
//     | UserId of uint32
//     | Certificate of byte[]
//     | CertificateAndUserId of certificate : byte[] * userId : uint32
//
// type AssociationResponseMode =
//     | ConfirmedAssociation
//     | UnconfirmedAssociation
//
// type InitiateRequestFields =
//     {
//         ResponseMode : AssociationResponseMode
//         ProposedDlmsVersionNumber : DlmsVersionNumber
//         ProposedConformance : ProposedConformance
//         ClientMaxReceivePduSize : ClientMaxReceivePduSize
//     }
//
// type CipheringKeyProposal =
//     | UseGlobalCipheringOnly
//     | UseDedicatedKey of byte[]
//
// type ClientUserIdentifier = ClientUserIdentifier of uint32
//
// type AcseAssociateTag =
//     | ProtocolVersion = 0x80uy
//     | SenderAcseRequirements = 0x8Auy
//     | MechanismName = 0x8Buy
//     | ImplementationInformation = 0x9Duy
//     | ApplicationContextName = 0xA1uy
//     | CalledApTitle = 0xA2uy
//     | CalledAeQualifier = 0xA3uy
//     | CalledApInvocationId = 0xA4uy
//     | CalledAeInvocationId = 0xA5uy
//     | CallingApTitle = 0xA6uy
//     | CallingAeQualifier = 0xA7uy
//     | CallingApInvocationId = 0xA8uy
//     | CallingAeInvocationId = 0xA9uy
//     | CallingAuthenticationValue = 0xACuy
//     | UserInformation = 0xBEuy
//
// module AcseAssociateFields =
//     type ProtocolVersion =
//         | Version1
//
//
//     module ProtocolVersion =
//         let private parseBody : Parser<ProtocolVersion> =
//             parser {
//                 let! bits = Ber.BitString.parseContent
//                 if bits.UnusedBitCount = 7uy
//                    && bits.Payload.Length = 1
//                    && bits.Payload.Span[0] = 0x80uy then
//                     return Version1
//                 else
//                     return! fail $"unsupported protocol-version value {bits}"
//         }
//
//         let parseOptional : Parser<ProtocolVersion option> =
//             withCtx "ProtocolVersion" <|
//             Ber.parseTaggedOptional AcseAssociateTag.ProtocolVersion parseBody
//
//     type ApplicationContextName =
//         | LogicalNameNoCiphering
//         | ShortNameNoCiphering
//         | LogicalNameWithCiphering
//         | ShortNameWithCiphering
//
//     module ApplicationContextName =
//         type ApplicationContextId =
//             | LnNoCiphering = 0x01uy
//             | SnNoCiphering = 0x02uy
//             | LnWithCiphering = 0x03uy
//             | SnWithCiphering = 0x04uy
//
//         let private applicationContextNameOid=
//             ReadOnlyMemory([| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x01uy |])
//
//         let private parseOidValue : Parser<ApplicationContextName> =
//             parser {
//                 do! expectMem applicationContextNameOid
//                 let! contextId = Tag.parse<ApplicationContextId>
//
//                 match contextId with
//                 | ApplicationContextId.LnNoCiphering -> return LogicalNameNoCiphering
//                 | ApplicationContextId.SnNoCiphering -> return ShortNameNoCiphering
//                 | ApplicationContextId.LnWithCiphering -> return LogicalNameWithCiphering
//                 | ApplicationContextId.SnWithCiphering -> return ShortNameWithCiphering
//                 | _ -> return! fail $"unsupported context_id {contextId}"
//             }
//
//         let parse : Parser<ApplicationContextName> =
//             withCtx "ApplicationContextName" <|
//             Ber.parseTagged
//                 AcseAssociateTag.ApplicationContextName
//                 (Ber.ObjectIdentifier.parseContent parseOidValue)
//
//     type ClientCertificate = Ber.OctetString
//
//     module CallingAeQualifier =
//         let parseOptional : Parser<ClientCertificate option> =
//             withCtx "CallingAeQualifier" <|
//             parser {
//                 let! qualifier = Ber.parseTaggedOptional AcseAssociateTag.CallingAeQualifier Ber.OctetString.parse
//                 return qualifier
//             }
//
//     type ClientUserIdentifier = ClientUserIdentifier of uint32
//
//     module CallingAeInvocationIdParser =
//         let parseOptional : Parser<ClientUserIdentifier option> =
//             withCtx "CallingAeInvocationId" <|
//             parser {
//                 let! identifier = Ber.parseTaggedOptional AcseAssociateTag.CallingAeInvocationId Ber.parseUint32
//                 return identifier |> Option.map ClientUserIdentifier
//             }
//
//     type HlsAuthenticationMechanismName =
//         | HighLevelSecurityManufacturerSpecific
//         | HighLevelSecurityMd5
//         | HighLevelSecuritySha1
//         | HighLevelSecurityGmac
//         | HighLevelSecuritySha256
//         | HighLevelSecurityEcdsa
//
//     type Authentication =
//         | LowestLevelSecurity
//         | LowLevelSecurity of authenticationValue : ReadOnlyMemory<byte>
//         | HighLevelSecurity of
//             mechanism : HlsAuthenticationMechanismName *
//             authenticationValue : ReadOnlyMemory<byte>
//
//     type AuthenticationMechanism =
//         | LowestLevelSecurity
//         | LowLevelSecurity
//         | HighLevelSecurity of HlsAuthenticationMechanismName
//
//     type AuthenticationMechanismName =
//         | LowestLevelSecurity = 0x00uy
//         | LowLevelSecurity = 0x01uy
//         | HighLevelSecurityManufacturerSpecific = 0x02uy
//         | HighLevelSecurityMd5 = 0x03uy
//         | HighLevelSecuritySha1 = 0x04uy
//         | HighLevelSecurityGmac = 0x05uy
//         | HighLevelSecuritySha256 = 0x06uy
//         | HighLevelSecurityEcdsa = 0x07uy
//
//     module AuthenticationMechanismName =
//         let private mechanismNameOid =
//             ReadOnlyMemory<byte>([| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x02uy |])
//
//         let private parseBody : Parser<AuthenticationMechanism> =
//             withCtx "MechanismName" <|
//             parser {
//                 do! expectMem mechanismNameOid
//                 let! mechanismName = Tag.parse<AuthenticationMechanismName>
//
//                 match mechanismName with
//                 | AuthenticationMechanismName.LowestLevelSecurity ->
//                     return LowestLevelSecurity
//                 | AuthenticationMechanismName.LowLevelSecurity ->
//                     return LowLevelSecurity
//                 | AuthenticationMechanismName.HighLevelSecurityManufacturerSpecific ->
//                     return HighLevelSecurity HighLevelSecurityManufacturerSpecific
//                 | AuthenticationMechanismName.HighLevelSecurityMd5 ->
//                     return HighLevelSecurity HighLevelSecurityMd5
//                 | AuthenticationMechanismName.HighLevelSecuritySha1 ->
//                     return HighLevelSecurity HighLevelSecuritySha1
//                 | AuthenticationMechanismName.HighLevelSecurityGmac ->
//                     return HighLevelSecurity HighLevelSecurityGmac
//                 | AuthenticationMechanismName.HighLevelSecuritySha256 ->
//                     return HighLevelSecurity HighLevelSecuritySha256
//                 | AuthenticationMechanismName.HighLevelSecurityEcdsa ->
//                     return HighLevelSecurity HighLevelSecurityEcdsa
//                 | _ ->
//                     return! fail $"unsupported authentication mechanism {mechanismName}"
//             }
//
//         let parse : Parser<AuthenticationMechanism> =
//             withCtx "AuthenticationMechanismName" <|
//                 Ber.parseTagged AcseAssociateTag.MechanismName parseBody
//
//     module CallingAuthenticationValue =
//         type AuthenticationValueTag =
//             | GraphicString = 0x80uy
//             | BitString = 0x81uy
//
//         let private parseAsCharString : Parser<ReadOnlyMemory<uint8>> =
//             parser {
//                 let! tag = Tag.parse<AuthenticationValueTag>
//                 match tag with
//                 | AuthenticationValueTag.GraphicString ->
//                     return! Ber.parseContentBytes
//                 | AuthenticationValueTag.BitString ->
//                     return! fail "BIT STRING authentication-value is supported by ASN.1 but not by this library"
//                 | _ ->
//                     return! fail $"unexpected tag in CallingAuthenticationValue: {tag}"
//             }
//
//         let parse : Parser<ReadOnlyMemory<byte>> =
//             withCtx "CallingAuthenticationValue" <|
//             Ber.parseTagged AcseAssociateTag.CallingAuthenticationValue parseAsCharString
//
//     type UserInformation = Ber.OctetString
//
//     module UserInformation =
//         let parse : Parser<UserInformation> =
//             withCtx "UserInformation" <|
//             Ber.parseTagged AcseAssociateTag.UserInformation Ber.OctetString.parse
//
// open AcseAssociateFields
//
// type LogicalNameNoCipheringAarq =
//     {
//         CallingAeQualifier : ClientCertificate option
//         CallingAeInvocationId : ClientUserIdentifier option
//         Authentication : Authentication
//         InitiateRequest: Ber.OctetString
//     }
//
// type SenderAcseRequirements =
//     | AuthenticationNotSelected
//     | AuthenticationSelected
//
// module SenderAcseRequirements =
//     let private isAuthenticationBitSet (bits: Ber.BitString) =
//         bits.Payload.Length > 0
//         && (bits.Payload.Span[0] &&& 0x80uy) <> 0uy
//
//     let private parseBody : Parser<SenderAcseRequirements> =
//         parser {
//             let! bits = Ber.BitString.parseContent
//
//             if isAuthenticationBitSet bits then
//                 return AuthenticationSelected
//             else
//                 return! fail "sender-acse-requirements present but authentication bit is not set"
//         }
//
//     let parse : Parser<SenderAcseRequirements> =
//         withCtx "SenderAcseRequirements" <|
//         parser {
//             let! value =
//                 Ber.parseTaggedOptional
//                     AcseAssociateTag.SenderAcseRequirements
//                     parseBody
//
//             return defaultArg value AuthenticationNotSelected
//         }
//
// module LogicalNameNoCipheringAarq =
//     let private parseAuthentication : Parser<Authentication> =
//         parser {
//             let! requirements = SenderAcseRequirements.parse
//             match requirements with
//             | AuthenticationNotSelected ->
//                 return Authentication.LowestLevelSecurity
//             | AuthenticationSelected ->
//                 let! mechanism = AuthenticationMechanismName.parse
//                 match mechanism with
//                 | LowestLevelSecurity ->
//                     return! fail "lowest-level-security mechanism is invalid when sender-acse-requirements selects authentication"
//                 | LowLevelSecurity ->
//                     let! value = CallingAuthenticationValue.parse
//                     return Authentication.LowLevelSecurity value
//                 | HighLevelSecurity kind ->
//                     let! value = CallingAuthenticationValue.parse
//                     return Authentication.HighLevelSecurity(kind, value)
//         }
//
//     let parseBody : Parser<LogicalNameNoCipheringAarq> =
//         parser {
//             let! qualifier = CallingAeQualifier.parseOptional
//             let! invocationId = CallingAeInvocationIdParser.parseOptional
//             let! Authentication = parseAuthentication
//             let! userInformation = UserInformation.parse
//             return {
//                 CallingAeQualifier = qualifier
//                 CallingAeInvocationId = invocationId
//                 Authentication = Authentication
//                 InitiateRequest = userInformation
//             }
//         }
//
// type ShortNameNoCipheringAarq =
//     {
//         ClientIdentity : ClientIdentity
//         Authentication : Authentication
//         InitiateRequest : InitiateRequestFields
//     }
//
// type LogicalNameWithCipheringAarq =
//     {
//         ClientSystemTitle : byte[]
//         ClientIdentity : ClientIdentity
//         Authentication : Authentication
//         KeyProposal : CipheringKeyProposal
//         InitiateRequest : InitiateRequestFields
//     }
//
// type ShortNameWithCipheringAarq =
//     {
//         ClientSystemTitle : byte[]
//         ClientIdentity : ClientIdentity
//         Authentication : Authentication
//         KeyProposal : CipheringKeyProposal
//         InitiateRequest : InitiateRequestFields
//     }
//
// type Aarq =
//     | LogicalNameNoCipheringAarq of LogicalNameNoCipheringAarq
//     | ShortNameNoCipheringAarq of ShortNameNoCipheringAarq
//     | LogicalNameWithCipheringAarq of LogicalNameWithCipheringAarq
//     | ShortNameWithCipheringAarq of ShortNameWithCipheringAarq
//
// module Aarq =
//     let private parsePdu : Parser<Aarq> =
//         parser {
//             let! _ = ProtocolVersion.parseOptional
//             let! contextName = ApplicationContextName.parse
//             match contextName with
//             | LogicalNameNoCiphering ->
//                 return! LogicalNameNoCipheringAarq.parseBody |>> LogicalNameNoCipheringAarq
//         }
//
//     let parseBody : Parser<Aarq> =
//         withCtx "AARQ" <| Ber.parseLengthDelimited parsePdu
//
// type Acse =
//     | Aarq of Aarq
//     | Aare of AareMsg
//     | Rlrq of RlrqMsg
//     | Rlre of RlreMsg
//
// type ApduTag =
//     | Aarq = 0x60uy
//     | Aare = 0x61uy
//     | Rlrq = 0x62uy
//     | Rlre = 0x63uy
//     | InitiateRequest = 0x01uy
//     | ReadRequest = 0x05uy
//     | WriteRequest = 0x06uy
//     | InitiateResponse = 0x08uy
//     | ReadResponse = 0x0Cuy
//     | WriteResponse = 0x0Duy
//     | ConfirmedServiceError = 0x0Euy
//     | DataNotification = 0x0Fuy
//     | DataNotificationConfirm = 0x10uy
//     | UnconfirmedWriteRequest = 0x16uy
//     | InformationReportRequest = 0x18uy
//     | GetRequest = 0xC0uy
//     | SetRequest = 0xC1uy
//     | ActionRequest = 0xC3uy
//     | GetResponse = 0xC4uy
//     | SetResponse = 0xC5uy
//     | ActionResponse = 0xC7uy
//     | ExceptionResponse = 0xD8uy
//     | AccessRequest = 0xD9uy
//     | AccessResponse = 0xDAuy
//     | GeneralGloCiphering = 0xDBuy
//     | GeneralDedCiphering = 0xDCuy
//     | GeneralCiphering = 0xDDuy
//     | GeneralSigning = 0xDFuy
//     | GeneralBlockTransfer = 0xE0uy
//
// type Apdu =
//     | Acse of Acse
//     | Xdlms of XdlmsApdu
//
// module Apdu =
//     let parse : Parser<Apdu> =
//         withCtx "Apdu" <|
//             parser {
//                 let! tag = Tag.parse<ApduTag>
//
//                 match tag with
//                 | ApduTag.Aarq ->
//                     return! withCtx "AARQ" Aarq.parseBody |>> Aarq |>> Acse
//
//                 | ApduTag.Aare ->
//                     let! msg = withCtx "AARE" parseAareBody
//                     return Acse (Aare msg)
//
//                 | ApduTag.Rlrq ->
//                     let! msg = withCtx "RLRQ" parseRlrqBody
//                     return Acse (Rlrq msg)
//
//                 | ApduTag.Rlre ->
//                     let! msg = withCtx "RLRE" parseRlreBody
//                     return Acse (Rlre msg)
//
//                 | ApduTag.InitiateRequest ->
//                     let! msg = withCtx "InitiateRequest" parseInitiateRequestBody
//                     return Xdlms (InitiateRequest msg)
//
//                 | other ->
//                     return! fail $"unsupported xDLMS APDU {other}"
//         }
