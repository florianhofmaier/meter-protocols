namespace DlmsMessages.CosemServices

open DlmsMessages.Acse.AssociateSourceDiagnosticRaw
open DlmsMessages.CosemServices.Open
open Metering.Common.Parsers.Core
open Metering.Dlms.Protocol

type ApduTag =
    | Aarq = 0x60uy
    | Aare = 0x61uy
    | Rlrq = 0x62uy
    | Rlre = 0x63uy
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
    | GetRequest = 0xC0uy
    | SetRequest = 0xC1uy
    | ActionRequest = 0xC3uy
    | GetResponse = 0xC4uy
    | SetResponse = 0xC5uy
    | ActionResponse = 0xC7uy
    | ExceptionResponse = 0xD8uy
    | AccessRequest = 0xD9uy
    | AccessResponse = 0xDAuy
    | GeneralGloCiphering = 0xDBuy
    | GeneralDedCiphering = 0xDCuy
    | GeneralCiphering = 0xDDuy
    | GeneralSigning = 0xDFuy
    | GeneralBlockTransfer = 0xE0uy

type CosemServiceRaw =
    | OpenRequest of OpenRequestRawFields
    | OpenResponse of OpenResponseRawFields
    | Xdlms of Xdlms.XdlmsApduRaw

module CosemServiceRaw =
    let parse : Parser<CosemServiceRaw> =
        parser {
            let! tag = Tag.peek<ApduTag>

            match tag with
            | ApduTag.Aarq ->
                return!
                    withCtx "AARQ" <|
                        OpenRequestRawFields.parseBody |>> OpenRequest

            | ApduTag.Aare ->
                return!
                    withCtx "AARE" <|
                    fail "AARE not supported"

            | ApduTag.Rlrq ->
                return!
                    withCtx "RLRQ" <|
                    fail "RLRQ not supported"

            | ApduTag.Rlre ->
                return!
                    withCtx "RLRE" <|
                    fail "RLRE not supported"

            | _ ->
                let! xdlms = Xdlms.XdlmsApduRaw.parse
                return Xdlms xdlms
        }

type CosemServiceError =
    | ParseError of PError
    | ValidationErrors of ValidationError list

type CosemService =
    | OpenRequest of OpenRequest
    | OpenResponse of OpenResponse
    | Xdlms of Xdlms.XdlmsApduRaw

module CosemService =
    let fromRaw (raw: CosemServiceRaw) : Validation<CosemService> =
        match raw with
        | CosemServiceRaw.OpenRequest request ->
            request
            |> OpenRequest.fromRaw
            |> Validation.map CosemService.OpenRequest

        | CosemServiceRaw.OpenResponse response ->
            response
            |> OpenResponse.fromRaw
            |> Validation.map CosemService.OpenResponse

        // | CosemServiceRaw.Xdlms xdlms ->
        //     xdlms
        //     |> Xdlms.Service.fromRaw
        //     |> Validation.map CosemService.Xdlms
