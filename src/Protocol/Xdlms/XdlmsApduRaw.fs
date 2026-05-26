namespace Metering.Dlms.Protocol.Xdlms

open System
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Xdlms.Common

type XdlmsApduRaw =
    | InitiateRequest of InitiateRequestRaw
    | GloInitiateRequest of Axdr.OctetString
    | DedInitiateRequest of ReadOnlyMemory<byte>

    | ReadRequest of ReadOnlyMemory<byte>
    | WriteRequest of ReadOnlyMemory<byte>
    | InitiateResponse of InitiateResponseRawFields
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

            // | XdlmsTag.ReadRequest ->
            //     return!
            //         withCtx "ReadRequest" <| takeAllMem |>> XdlmsApduRaw.ReadRequest
            //
            // | XdlmsTag.WriteRequest ->
            //     return!
            //         withCtx "WriteRequest" <| takeAllMem |>> XdlmsApduRaw.WriteRequest
            //
            // | XdlmsTag.InitiateResponse ->
            //     return!
            //         withCtx "InitiateResponse" <| InitiateResponseRawFields.parseBody |>> XdlmsApduRaw.InitiateResponse
            //
            // | XdlmsTag.ReadResponse ->
            //     return!
            //         withCtx "ReadResponse" <| takeAllMem |>> XdlmsApduRaw.ReadResponse
            //
            // | XdlmsTag.WriteResponse ->
            //     return!
            //         withCtx "WriteResponse" <| takeAllMem |>> XdlmsApduRaw.WriteResponse
            //
            // | XdlmsTag.ConfirmedServiceError ->
            //     return!
            //         withCtx "ConfirmedServiceError" <| ConfirmedServiceErrorRaw.parseBody |>> XdlmsApduRaw.ConfirmedServiceError
            //
            // | XdlmsTag.DataNotification ->
            //     return!
            //         withCtx "DataNotification" <| takeAllMem |>> XdlmsApduRaw.DataNotification
            //
            // | XdlmsTag.DataNotificationConfirm ->
            //     return!
            //         withCtx "DataNotificationConfirm" <| takeAllMem |>> XdlmsApduRaw.DataNotificationConfirm
            //
            // | XdlmsTag.UnconfirmedWriteRequest ->
            //     return!
            //         withCtx "UnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.UnconfirmedWriteRequest
            //
            // | XdlmsTag.InformationReportRequest ->
            //     return!
            //         withCtx "InformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.InformationReportRequest
            //
            // | XdlmsTag.GloReadRequest ->
            //     return!
            //         withCtx "GloReadRequest" <| takeAllMem |>> XdlmsApduRaw.GloReadRequest
            //
            // | XdlmsTag.GloWriteRequest ->
            //     return!
            //         withCtx "GloWriteRequest" <| takeAllMem |>> XdlmsApduRaw.GloWriteRequest
            //
            // | XdlmsTag.GloInitiateResponse ->
            //     return!
            //         withCtx "GloInitiateResponse" <| Axdr.OctetString.parse |>> XdlmsApduRaw.GloInitiateResponse
            //
            // | XdlmsTag.GloReadResponse ->
            //     return!
            //         withCtx "GloReadResponse" <| takeAllMem |>> XdlmsApduRaw.GloReadResponse
            //
            // | XdlmsTag.GloWriteResponse ->
            //     return!
            //         withCtx "GloWriteResponse" <| takeAllMem |>> XdlmsApduRaw.GloWriteResponse
            //
            // | XdlmsTag.GloConfirmedServiceError ->
            //     return!
            //         withCtx "GloConfirmedServiceError" <| takeAllMem |>> XdlmsApduRaw.GloConfirmedServiceError
            //
            // | XdlmsTag.GloUnconfirmedWriteRequest ->
            //     return!
            //         withCtx "GloUnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.GloUnconfirmedWriteRequest
            //
            // | XdlmsTag.GloInformationReportRequest ->
            //     return!
            //         withCtx "GloInformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.GloInformationReportRequest
            //
            // | XdlmsTag.DedInitiateRequest ->
            //     return!
            //         withCtx "DedInitiateRequest" <| Axdr.OctetString.parseAsBufferSlice |>> XdlmsApduRaw.DedInitiateRequest
            //
            // | XdlmsTag.DedReadRequest ->
            //     return!
            //         withCtx "DedReadRequest" <| takeAllMem |>> XdlmsApduRaw.DedReadRequest
            //
            // | XdlmsTag.DedWriteRequest ->
            //     return!
            //         withCtx "DedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.DedWriteRequest
            //
            // | XdlmsTag.DedInitiateResponse ->
            //     return!
            //         withCtx "DedInitiateResponse" <| Axdr.OctetString.parse |>> XdlmsApduRaw.DedInitiateResponse
            //
            // | XdlmsTag.DedReadResponse ->
            //     return!
            //         withCtx "DedReadResponse" <| takeAllMem |>> XdlmsApduRaw.DedReadResponse
            //
            // | XdlmsTag.DedWriteResponse ->
            //     return!
            //         withCtx "DedWriteResponse" <| takeAllMem |>> XdlmsApduRaw.DedWriteResponse
            //
            // | XdlmsTag.DedConfirmedServiceError ->
            //     return!
            //         withCtx "DedConfirmedServiceError" <| takeAllMem |>> XdlmsApduRaw.DedConfirmedServiceError
            //
            // | XdlmsTag.DedUnconfirmedWriteRequest ->
            //     return!
            //         withCtx "DedUnconfirmedWriteRequest" <| takeAllMem |>> XdlmsApduRaw.DedUnconfirmedWriteRequest
            //
            // | XdlmsTag.DedInformationReportRequest ->
            //     return!
            //         withCtx "DedInformationReportRequest" <| takeAllMem |>> XdlmsApduRaw.DedInformationReportRequest
            //
            // | XdlmsTag.GetRequest ->
            //     return!
            //         withCtx "GetRequest" <| takeAllMem |>> XdlmsApduRaw.GetRequest
            //
            // | XdlmsTag.SetRequest ->
            //     return!
            //         withCtx "SetRequest" <| takeAllMem |>> XdlmsApduRaw.SetRequest
            //
            // | XdlmsTag.EventNotificationRequest ->
            //     return!
            //         withCtx "EventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.EventNotificationRequest
            //
            // | XdlmsTag.ActionRequest ->
            //     return!
            //         withCtx "ActionRequest" <| takeAllMem |>> XdlmsApduRaw.ActionRequest
            //
            // | XdlmsTag.GetResponse ->
            //     return!
            //         withCtx "GetResponse" <| takeAllMem |>> XdlmsApduRaw.GetResponse
            //
            // | XdlmsTag.SetResponse ->
            //     return!
            //         withCtx "SetResponse" <| takeAllMem |>> XdlmsApduRaw.SetResponse
            //
            // | XdlmsTag.ActionResponse ->
            //     return!
            //         withCtx "ActionResponse" <| takeAllMem |>> XdlmsApduRaw.ActionResponse
            //
            // | XdlmsTag.GloGetRequest ->
            //     return!
            //         withCtx "GloGetRequest" <| takeAllMem |>> XdlmsApduRaw.GloGetRequest
            //
            // | XdlmsTag.GloSetRequest ->
            //     return!
            //         withCtx "GloSetRequest" <| takeAllMem |>> XdlmsApduRaw.GloSetRequest
            //
            // | XdlmsTag.GloEventNotificationRequest ->
            //     return!
            //         withCtx "GloEventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.GloEventNotificationRequest
            //
            // | XdlmsTag.GloActionRequest ->
            //     return!
            //         withCtx "GloActionRequest" <| takeAllMem |>> XdlmsApduRaw.GloActionRequest
            //
            // | XdlmsTag.GloGetResponse ->
            //     return!
            //         withCtx "GloGetResponse" <| takeAllMem |>> XdlmsApduRaw.GloGetResponse
            //
            // | XdlmsTag.GloSetResponse ->
            //     return!
            //         withCtx "GloSetResponse" <| takeAllMem |>> XdlmsApduRaw.GloSetResponse
            //
            // | XdlmsTag.GloActionResponse ->
            //     return!
            //         withCtx "GloActionResponse" <| takeAllMem |>> XdlmsApduRaw.GloActionResponse
            //
            // | XdlmsTag.DedGetRequest ->
            //     return!
            //         withCtx "DedGetRequest" <| takeAllMem |>> XdlmsApduRaw.DedGetRequest
            //
            // | XdlmsTag.DedSetRequest ->
            //     return!
            //         withCtx "DedSetRequest" <| takeAllMem |>> XdlmsApduRaw.DedSetRequest
            //
            // | XdlmsTag.DedEventNotificationRequest ->
            //     return!
            //         withCtx "DedEventNotificationRequest" <| takeAllMem |>> XdlmsApduRaw.DedEventNotificationRequest
            //
            // | XdlmsTag.DedActionRequest ->
            //     return!
            //         withCtx "DedActionRequest" <| takeAllMem |>> XdlmsApduRaw.DedActionRequest
            //
            // | XdlmsTag.DedGetResponse ->
            //     return!
            //         withCtx "DedGetResponse" <| takeAllMem |>> XdlmsApduRaw.DedGetResponse
            //
            // | XdlmsTag.DedSetResponse ->
            //     return!
            //         withCtx "DedSetResponse" <| takeAllMem |>> XdlmsApduRaw.DedSetResponse
            //
            // | XdlmsTag.DedActionResponse ->
            //     return!
            //         withCtx "DedActionResponse" <| takeAllMem |>> XdlmsApduRaw.DedActionResponse
            //
            // | XdlmsTag.ExceptionResponse ->
            //     return!
            //         withCtx "ExceptionResponse" <| takeAllMem |>> XdlmsApduRaw.ExceptionResponse
            //
            // | XdlmsTag.AccessRequest ->
            //     return!
            //         withCtx "AccessRequest" <| takeAllMem |>> XdlmsApduRaw.AccessRequest
            //
            // | XdlmsTag.AccessResponse ->
            //     return!
            //         withCtx "AccessResponse" <| takeAllMem |>> XdlmsApduRaw.AccessResponse
            //
            // | XdlmsTag.GeneralGloCiphering ->
            //     return!
            //         withCtx "GeneralGloCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralGloCiphering
            //
            // | XdlmsTag.GeneralDedCiphering ->
            //     return!
            //         withCtx "GeneralDedCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralDedCiphering
            //
            // | XdlmsTag.GeneralCiphering ->
            //     return!
            //         withCtx "GeneralCiphering" <| takeAllMem |>> XdlmsApduRaw.GeneralCiphering
            //
            // | XdlmsTag.GeneralSigning ->
            //     return!
            //         withCtx "GeneralSigning" <| takeAllMem |>> XdlmsApduRaw.GeneralSigning
            //
            // | XdlmsTag.GeneralBlockTransfer ->
            //     return!
            //         withCtx "GeneralBlockTransfer" <| takeAllMem |>> XdlmsApduRaw.GeneralBlockTransfer

