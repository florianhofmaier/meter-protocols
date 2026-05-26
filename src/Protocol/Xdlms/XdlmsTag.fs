namespace Metering.Dlms.Protocol.Xdlms

type XdlmsTag =
    // Without ciphering: base / SN-related APDUs
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

    // Service-specific global ciphering: base / SN-related APDUs
    | GloInitiateRequest = 0x21uy
    | GloReadRequest = 0x25uy
    | GloWriteRequest = 0x26uy

    | GloInitiateResponse = 0x28uy
    | GloReadResponse = 0x2Cuy
    | GloWriteResponse = 0x2Duy

    | GloConfirmedServiceError = 0x2Euy

    | GloUnconfirmedWriteRequest = 0x36uy
    | GloInformationReportRequest = 0x38uy

    // Service-specific dedicated ciphering: base / SN-related APDUs
    | DedInitiateRequest = 0x41uy
    | DedReadRequest = 0x45uy
    | DedWriteRequest = 0x46uy

    | DedInitiateResponse = 0x48uy
    | DedReadResponse = 0x4Cuy
    | DedWriteResponse = 0x4Duy

    | DedConfirmedServiceError = 0x4Euy

    | DedUnconfirmedWriteRequest = 0x56uy
    | DedInformationReportRequest = 0x58uy

    // Without ciphering: LN APDUs
    | GetRequest = 0xC0uy
    | SetRequest = 0xC1uy
    | EventNotificationRequest = 0xC2uy
    | ActionRequest = 0xC3uy

    | GetResponse = 0xC4uy
    | SetResponse = 0xC5uy
    | ActionResponse = 0xC7uy

    // Service-specific global ciphering: LN APDUs
    | GloGetRequest = 0xC8uy
    | GloSetRequest = 0xC9uy
    | GloEventNotificationRequest = 0xCAuy
    | GloActionRequest = 0xCBuy

    | GloGetResponse = 0xCCuy
    | GloSetResponse = 0xCDuy
    | GloActionResponse = 0xCFuy

    // Service-specific dedicated ciphering: LN APDUs
    | DedGetRequest = 0xD0uy
    | DedSetRequest = 0xD1uy
    | DedEventNotificationRequest = 0xD2uy
    | DedActionRequest = 0xD3uy

    | DedGetResponse = 0xD4uy
    | DedSetResponse = 0xD5uy
    | DedActionResponse = 0xD7uy

    // Exception / Access
    | ExceptionResponse = 0xD8uy

    | AccessRequest = 0xD9uy
    | AccessResponse = 0xDAuy

    // General APDUs
    | GeneralGloCiphering = 0xDBuy
    | GeneralDedCiphering = 0xDCuy
    | GeneralCiphering = 0xDDuy
    | GeneralSigning = 0xDFuy
    | GeneralBlockTransfer = 0xE0uy