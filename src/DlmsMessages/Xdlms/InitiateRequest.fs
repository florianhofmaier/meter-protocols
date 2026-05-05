module DlmsMessages.Xdlms

open DlmsMessages.Xdlms
open Mbus.BaseParsers.Core
open Validators

type InitiateRequestRaw =
    {
        DedicatedKey : Axdr.Optional<Axdr.OctetString>
        ResponseAllowed : Axdr.Default<Axdr.Boolean>
        ProposedQualityOfService : Axdr.Optional<Axdr.Integer8>
        ProposedDlmsVersionNumber : Axdr.Unsigned8
        ProposedConformance : ConformanceRaw
        ClientMaxReceivePduSize : Axdr.Unsigned16
    }

module InitiateRequestRaw =
    let parseBody : Parser<InitiateRequestRaw> =
        parser {
            let! dedicatedKey =
                withCtx "DedicatedKey" <|
                Axdr.Optional.parse Axdr.OctetString.parse

            let! responseAllowed =
                withCtx "ResponseAllowed" <|
                Axdr.Default.parse Axdr.Boolean.parse

            let! proposedQualityOfService =
                withCtx "ProposedQualityOfService" <|
                Axdr.Optional.parse Axdr.Integer8.parse

            let! proposedDlmsVersionNumber =
                withCtx "ProposedDlmsVersionNumber" <|
                Axdr.Unsigned8.parse

            let! proposedConformance =
                ConformanceRaw.parse

            let! clientMaxReceivePduSize =
                withCtx "ClientMaxReceivePduSize" <|
                Axdr.Unsigned16.parse

            return {
                DedicatedKey = dedicatedKey
                ResponseAllowed = responseAllowed
                ProposedQualityOfService = proposedQualityOfService
                ProposedDlmsVersionNumber = proposedDlmsVersionNumber
                ProposedConformance = proposedConformance
                ClientMaxReceivePduSize = clientMaxReceivePduSize
            }
        }

    let parse : Parser<InitiateRequestRaw> =
        parser {
            do! Tag.expect XdlmsTag.InitiateRequest
            return! parseBody
        }

type DedicatedKey =
    private DedicatedKey of Axdr.OctetString

module DedicatedKey =
    let create value = DedicatedKey value
    let value (DedicatedKey value) = value

type ResponseAllowed =
    | ResponseAllowed
    | ResponseNotAllowed

module ResponseAllowed =

    let private fromBool value =
        if value then
            ResponseAllowed
        else
            ResponseNotAllowed

    let validate
        (raw: Axdr.Default<Axdr.Boolean>)
        : Validation<ResponseAllowed> =

        raw
        |> Axdr.Default.valueOrDefault (Axdr.Boolean.create true)
        |> Axdr.Boolean.value
        |> fromBool
        |> Validation.ok

type ProposedQualityOfService =
    private ProposedQualityOfService of Axdr.Integer8

module ProposedQualityOfService =
    let create value = ProposedQualityOfService value
    let value (ProposedQualityOfService value) = value

type DlmsVersionNumber =
    private DlmsVersionNumber of Axdr.Unsigned8

module DlmsVersionNumber =
    let create value = DlmsVersionNumber value
    let value (DlmsVersionNumber value) = value

type ClientMaxReceivePduSize =
    private ClientMaxReceivePduSize of Axdr.Unsigned16

module ClientMaxReceivePduSize =
    let create value = ClientMaxReceivePduSize value
    let value (ClientMaxReceivePduSize value) = value

type InitiateRequest =
    {
        DedicatedKey : DedicatedKey option
        ResponseAllowed : ResponseAllowed
        ProposedQualityOfService : ProposedQualityOfService option
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : Conformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module InitiateRequest =
    let fromRaw (raw: InitiateRequestRaw) : Validation<InitiateRequest> =
        validator {
            let dedicatedKey =
                raw.DedicatedKey
                |> Axdr.Optional.toOption DedicatedKey.create

            let proposedQualityOfService =
                raw.ProposedQualityOfService
                |> Axdr.Optional.toOption ProposedQualityOfService.create

            let proposedDlmsVersionNumber =
                raw.ProposedDlmsVersionNumber
                |> DlmsVersionNumber.create

            let clientMaxReceivePduSize =
                raw.ClientMaxReceivePduSize
                |> ClientMaxReceivePduSize.create

            let! responseAllowed =
                ResponseAllowed.validate raw.ResponseAllowed

            and! proposedConformance =
                Conformance.validate raw.ProposedConformance

            return {
                DedicatedKey = dedicatedKey
                ResponseAllowed = responseAllowed
                ProposedQualityOfService = proposedQualityOfService
                ProposedDlmsVersionNumber = proposedDlmsVersionNumber
                ProposedConformance = proposedConformance
                ClientMaxReceivePduSize = clientMaxReceivePduSize
            }
        }