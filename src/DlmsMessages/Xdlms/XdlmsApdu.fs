module DlmsMessages.Xdlms.XdlmsApdu

open System
open DlmsMessages
open Mbus.BaseParsers.Core

type XdlmsTag =
    | InitiateRequest = 0x01uy
    | ReadRequest = 0x05uy
    | WriteRequest = 0x06uy
    | InitiateResponse = 0x08uy
    | ReadResponse = 0x0Cuy
    | WriteResponse = 0x0Duy
    | ConfirmedServiceError = 0x0Euy
    | DataNotification = 0x0Fuy
    | DataNotificationConfirm = 0x10uy
    | UnconfirmedWriteRequest = 0x16uy
    | InformationReportRequest = 0x18uy
    | GloInitiateRequest = 0x21uy
    | GloReadRequest = 0x25uy
    | GloWriteRequest = 0x26uy
    | GloInitiateResponse = 0x28uy
    | GloReadResponse = 0x2Cuy
    | GloWriteResponse = 0x2Duy
    | GloConfirmedServiceError = 0x2Euy
    | GloUnconfirmedWriteRequest = 0x36uy
    | GloInformationReportRequest = 0x38uy
    | DedInitiateRequest = 0x41uy
    | DedReadRequest = 0x45uy
    | DedWriteRequest = 0x46uy
    | DedInitiateResponse = 0x48uy
    | DedReadResponse = 0x4Cuy
    | DedWriteResponse = 0x4Duy
    | DedConfirmedServiceError = 0x4Euy
    | DedUnconfirmedWriteRequest = 0x56uy
    | DedInformationReportRequest = 0x58uy
    | GetRequest = 0xC0uy
    | SetRequest = 0xC1uy
    | EventNotificationRequest = 0xC2uy
    | ActionRequest = 0xC3uy
    | GetResponse = 0xC4uy
    | SetResponse = 0xC5uy
    | ActionResponse = 0xC7uy
    | GloGetRequest = 0xC8uy
    | GloSetRequest = 0xC9uy
    | GloEventNotificationRequest = 0xCAuy
    | GloActionRequest = 0xCBuy
    | GloGetResponse = 0xCCuy
    | GloSetResponse = 0xCDuy
    | GloActionResponse = 0xCFuy
    | DedGetRequest = 0xD0uy
    | DedSetRequest = 0xD1uy
    | DedEventNotificationRequest = 0xD2uy
    | DedActionRequest = 0xD3uy
    | DedGetResponse = 0xD4uy
    | DedSetResponse = 0xD5uy
    | DedActionResponse = 0xD7uy
    | ExceptionResponse = 0xD8uy
    | AccessRequest = 0xD9uy
    | AccessResponse = 0xDAuy
    | GeneralGloCiphering = 0xDBuy
    | GeneralDedCiphering = 0xDCuy
    | GeneralCiphering = 0xDDuy
    | GeneralSigning = 0xDFuy
    | GeneralBlockTransfer = 0xE0uy

type XdlmsApduRaw =
    | InitiateRequest of InitiateRequestRaw
    | GloInitiateRequest of Axdr.OctetString
    | DedInitiateRequest of Axdr.OctetString

    | ReadRequest of ReadOnlyMemory<byte>
    | WriteRequest of ReadOnlyMemory<byte>
    | InitiateResponse of InitiateResponseRaw
    | ReadResponse of ReadOnlyMemory<byte>
    | WriteResponse of ReadOnlyMemory<byte>
    | ConfirmedServiceError of ConfirmedServiceErrorRaw
    | DataNotification of ReadOnlyMemory<byte>
    | DataNotificationConfirm of ReadOnlyMemory<byte>
    | UnconfirmedWriteRequest of ReadOnlyMemory<byte>
    | InformationReportRequest of ReadOnlyMemory<byte>

    | GloReadRequest of ReadOnlyMemory<byte>
    | GloWriteRequest of ReadOnlyMemory<byte>
    | GloInitiateResponse of Axdr.OctetString
    | GloReadResponse of ReadOnlyMemory<byte>
    | GloWriteResponse of ReadOnlyMemory<byte>
    | GloConfirmedServiceError of ReadOnlyMemory<byte>
    | GloUnconfirmedWriteRequest of ReadOnlyMemory<byte>
    | GloInformationReportRequest of ReadOnlyMemory<byte>

    | DedReadRequest of ReadOnlyMemory<byte>
    | DedWriteRequest of ReadOnlyMemory<byte>
    | DedInitiateResponse of Axdr.OctetString
    | DedReadResponse of ReadOnlyMemory<byte>
    | DedWriteResponse of ReadOnlyMemory<byte>
    | DedConfirmedServiceError of ReadOnlyMemory<byte>
    | DedUnconfirmedWriteRequest of ReadOnlyMemory<byte>
    | DedInformationReportRequest of ReadOnlyMemory<byte>
    | GetRequest of ReadOnlyMemory<byte>
    | SetRequest of ReadOnlyMemory<byte>
    | EventNotificationRequest of ReadOnlyMemory<byte>
    | ActionRequest of ReadOnlyMemory<byte>
    | GetResponse of ReadOnlyMemory<byte>
    | SetResponse of ReadOnlyMemory<byte>
    | ActionResponse of ReadOnlyMemory<byte>
    | GloGetRequest of ReadOnlyMemory<byte>
    | GloSetRequest of ReadOnlyMemory<byte>
    | GloEventNotificationRequest of ReadOnlyMemory<byte>
    | GloActionRequest of ReadOnlyMemory<byte>
    | GloGetResponse of ReadOnlyMemory<byte>
    | GloSetResponse of ReadOnlyMemory<byte>
    | GloActionResponse of ReadOnlyMemory<byte>
    | DedGetRequest of ReadOnlyMemory<byte>
    | DedSetRequest of ReadOnlyMemory<byte>
    | DedEventNotificationRequest of ReadOnlyMemory<byte>
    | DedActionRequest of ReadOnlyMemory<byte>
    | DedGetResponse of ReadOnlyMemory<byte>
    | DedSetResponse of ReadOnlyMemory<byte>
    | DedActionResponse of ReadOnlyMemory<byte>
    | ExceptionResponse of ReadOnlyMemory<byte>
    | AccessRequest of ReadOnlyMemory<byte>
    | AccessResponse of ReadOnlyMemory<byte>
    | GeneralGloCiphering of ReadOnlyMemory<byte>
    | GeneralDedCiphering of ReadOnlyMemory<byte>
    | GeneralCiphering of ReadOnlyMemory<byte>
    | GeneralSigning of ReadOnlyMemory<byte>
    | GeneralBlockTransfer of ReadOnlyMemory<byte>

module XdlmsApduRaw =
    let parse : Parser<XdlmsApduRaw> =
        parser {
            let! tag = Tag.parse<XdlmsTag>

            match tag with
            | XdlmsTag.InitiateRequest ->
                return!
                    withCtx "InitiateRequest" <| InitiateRequestRaw.parseBody |>> XdlmsApduRaw.InitiateRequest

            | XdlmsTag.ReadRequest ->
                return!
                    withCtx "ReadRequest" <| takeAllMem |>> XdlmsApduRaw.ReadRequest

            | XdlmsTag.WriteRequest ->
                return!
                    withCtx "WriteRequest" <| takeAllMem |>> XdlmsApduRaw.WriteRequest

            | XdlmsTag.InitiateResponse ->
                return!
                    withCtx "InitiateResponse" <| InitiateResponseRaw.parseBody |>> XdlmsApduRaw.InitiateResponse

            | XdlmsTag.ReadResponse ->
                return!
                    withCtx "ReadResponse" <| takeAllMem |>> XdlmsApduRaw.ReadResponse

            | XdlmsTag.WriteResponse ->
                return!
                    withCtx "WriteResponse" <| takeAllMem |>> XdlmsApduRaw.WriteResponse

            | XdlmsTag.ConfirmedServiceError ->
                return!
                    withCtx "ConfirmedServiceError" <| ConfirmedServiceErrorRaw.parseBody |>> XdlmsApduRaw.ConfirmedServiceError

            | XdlmsTag.DataNotification ->
                return!
                    withCtx "DataNotification" <| takeAllMem |>> XdlmsApduRaw.DataNotification

            | XdlmsTag.DataNotificationConfirm ->
                return!
                    withCtx "DataNotificationConfirm" <| takeAllMem |>> XdlmsApduRaw.DataNotificationConfirm

            | XdlmsTag.UnconfirmedWriteRequest ->
                return!
                    withCtx "UnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.UnconfirmedWriteRequest

            | XdlmsTag.InformationReportRequest ->
                return!
                    withCtx "InformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.InformationReportRequest

            | XdlmsTag.GloInitiateRequest ->
                return!
                    withCtx "GloInitiateRequest" <| Axdr.OctetString.parse |>> XdlmsApduRaw.GloInitiateRequest

            | XdlmsTag.GloReadRequest ->
                return!
                    withCtx "GloReadRequest" <| takeAllMem |>> XdlmsApduRaw.GloReadRequest

            | XdlmsTag.GloWriteRequest ->
                return!
                    withCtx "GloWriteRequest" <| takeAllMem |>> XdlmsApduRaw.GloWriteRequest

            | XdlmsTag.GloInitiateResponse ->
                return!
                    withCtx "GloInitiateResponse" <| Axdr.OctetString.parse |>> XdlmsApduRaw.GloInitiateResponse

            | XdlmsTag.GloReadResponse ->
                return!
                    withCtx "GloReadResponse" <| takeAllMem |>> XdlmsApduRaw.GloReadResponse

            | XdlmsTag.GloWriteResponse ->
                return!
                    withCtx "GloWriteResponse" <| takeAllMem |>> XdlmsApduRaw.GloWriteResponse

            | XdlmsTag.GloConfirmedServiceError ->
                return!
                    withCtx "GloConfirmedServiceError" <| takeAllMem |>> XdlmsApduRaw.GloConfirmedServiceError

            | XdlmsTag.GloUnconfirmedWriteRequest ->
                return!
                    withCtx "GloUnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.GloUnconfirmedWriteRequest

            | XdlmsTag.GloInformationReportRequest ->
                return!
                    withCtx "GloInformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.GloInformationReportRequest

            | XdlmsTag.DedInitiateRequest ->
                return!
                    withCtx "DedInitiateRequest" <| Axdr.OctetString.parse |>> XdlmsApduRaw.DedInitiateRequest

            | XdlmsTag.DedReadRequest ->
                return!
                    withCtx "DedReadRequest" <| takeAllMem |>> XdlmsApduRaw.DedReadRequest

            | XdlmsTag.DedWriteRequest ->
                return!
                    withCtx "DedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.DedWriteRequest

            | XdlmsTag.DedInitiateResponse ->
                return!
                    withCtx "DedInitiateResponse" <| Axdr.OctetString.parse |>> XdlmsApduRaw.DedInitiateResponse

            | XdlmsTag.DedReadResponse ->
                return!
                    withCtx "DedReadResponse" <| takeAllMem |>> XdlmsApduRaw.DedReadResponse

            | XdlmsTag.DedWriteResponse ->
                return!
                    withCtx "DedWriteResponse" <| takeAllMem |>> XdlmsApduRaw.DedWriteResponse

            | XdlmsTag.DedConfirmedServiceError ->
                return!
                    withCtx "DedConfirmedServiceError" <| takeAllMem |>> XdlmsApduRaw.DedConfirmedServiceError

            | XdlmsTag.DedUnconfirmedWriteRequest ->
                return!
                    withCtx "DedUnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.DedUnconfirmedWriteRequest

            | XdlmsTag.DedInformationReportRequest ->
                return!
                    withCtx "DedInformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.DedInformationReportRequest

            | XdlmsTag.GetRequest ->
                return!
                    withCtx "GetRequest" <| takeAllMem |>> XdlmsApduRaw.GetRequest

            | XdlmsTag.SetRequest ->
                return!
                    withCtx "SetRequest" <| takeAllMem |>> XdlmsApduRaw.SetRequest

            | XdlmsTag.EventNotificationRequest ->
                return!
                    withCtx "EventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.EventNotificationRequest

            | XdlmsTag.ActionRequest ->
                return!
                    withCtx "ActionRequest" <| takeAllMem |>> XdlmsApduRaw.ActionRequest

            | XdlmsTag.GetResponse ->
                return!
                    withCtx "GetResponse" <| takeAllMem |>> XdlmsApduRaw.GetResponse

            | XdlmsTag.SetResponse ->
                return!
                    withCtx "SetResponse" <| takeAllMem |>> XdlmsApduRaw.SetResponse

            | XdlmsTag.ActionResponse ->
                return!
                    withCtx "ActionResponse" <| takeAllMem |>> XdlmsApduRaw.ActionResponse

            | XdlmsTag.GloGetRequest ->
                return!
                    withCtx "GloGetRequest" <| takeAllMem |>> XdlmsApduRaw.GloGetRequest

            | XdlmsTag.GloSetRequest ->
                return!
                    withCtx "GloSetRequest" <| takeAllMem |>> XdlmsApduRaw.GloSetRequest

            | XdlmsTag.GloEventNotificationRequest ->
                return!
                    withCtx "GloEventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.GloEventNotificationRequest

            | XdlmsTag.GloActionRequest ->
                return!
                    withCtx "GloActionRequest" <| takeAllMem |>> XdlmsApduRaw.GloActionRequest

            | XdlmsTag.GloGetResponse ->
                return!
                    withCtx "GloGetResponse" <| takeAllMem |>> XdlmsApduRaw.GloGetResponse

            | XdlmsTag.GloSetResponse ->
                return!
                    withCtx "GloSetResponse" <| takeAllMem |>> XdlmsApduRaw.GloSetResponse

            | XdlmsTag.GloActionResponse ->
                return!
                    withCtx "GloActionResponse" <| takeAllMem |>> XdlmsApduRaw.GloActionResponse

            | XdlmsTag.DedGetRequest ->
                return!
                    withCtx "DedGetRequest" <| takeAllMem |>> XdlmsApduRaw.DedGetRequest

            | XdlmsTag.DedSetRequest ->
                return!
                    withCtx "DedSetRequest" <| takeAllMem |>> XdlmsApduRaw.DedSetRequest

            | XdlmsTag.DedEventNotificationRequest ->
                return!
                    withCtx "DedEventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.DedEventNotificationRequest

            | XdlmsTag.DedActionRequest ->
                return!
                    withCtx "DedActionRequest" <| takeAllMem |>> XdlmsApduRaw.DedActionRequest

            | XdlmsTag.DedGetResponse ->
                return!
                    withCtx "DedGetResponse" <| takeAllMem |>> XdlmsApduRaw.DedGetResponse

            | XdlmsTag.DedSetResponse ->
                return!
                    withCtx "DedSetResponse" <| takeAllMem |>> XdlmsApduRaw.DedSetResponse

            | XdlmsTag.DedActionResponse ->
                return!
                    withCtx "DedActionResponse" <| takeAllMem |>> XdlmsApduRaw.DedActionResponse

            | XdlmsTag.ExceptionResponse ->
                return!
                    withCtx "ExceptionResponse" <| takeAllMem |>> XdlmsApduRaw.ExceptionResponse

            | XdlmsTag.AccessRequest ->
                return!
                    withCtx "AccessRequest" <| takeAllMem |>> XdlmsApduRaw.AccessRequest

            | XdlmsTag.AccessResponse ->
                return!
                    withCtx "AccessResponse" <| takeAllMem |>> XdlmsApduRaw.AccessResponse

            | XdlmsTag.GeneralGloCiphering ->
                return!
                    withCtx "GeneralGloCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralGloCiphering

            | XdlmsTag.GeneralDedCiphering ->
                return!
                    withCtx "GeneralDedCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralDedCiphering

            | XdlmsTag.GeneralCiphering ->
                return!
                    withCtx "GeneralCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralCiphering

            | XdlmsTag.GeneralSigning ->
                return!
                    withCtx "GeneralSigning" <| takeAllMem |>> XdlmsApduRaw.GeneralSigning

            | XdlmsTag.GeneralBlockTransfer ->
                return!
                    withCtx "GeneralBlockTransfer" <| takeAllMem |>> XdlmsApduRaw.GeneralBlockTransfer

            | other ->
                return! fail $"unsupported xDLMS APDU-Tag {other}"
        }