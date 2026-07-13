namespace Metering.Dlms.Protocol.ApplicationLayer

open Metering.Dlms.Protocol.Acse.Aarq
open Metering.Dlms.Protocol.Acse.Fields
open Metering.Dlms.Protocol.Xdlms

type CosemOpenIndication =
    {
        Address : CosemAddress
        Aarq : AarqValidatedFields
        InitiateRequest : InitiateRequestValidatedFields
    }

type CosemOpenAccept =
    {
        Address : CosemAddress
        ApplicationContextName : ApplicationContextName
        InitiateResponse : InitiateResponseValidatedFields
    }

type CosemOpenRejectReason =
    | ApplicationContextNameNotSupported
    | AuthenticationMechanismNameNotSupported
    | AuthenticationFailed
    | XdlmsContextNotAccepted
    | UserNotKnown

type CosemOpenReject =
    {
        Address : CosemAddress
        Reason : CosemOpenRejectReason
    }

type CosemOpenResponse =
    | Accept of CosemOpenAccept
    | Reject of CosemOpenReject