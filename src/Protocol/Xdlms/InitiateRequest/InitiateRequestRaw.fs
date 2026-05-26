module Metering.Dlms.Protocol.Xdlms

open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators
open Metering.Common.Validators.Core
open Metering.Common.Validators.Fields
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.InitiateRequest

type InitiateRequestRaw =
    {
        DedicatedKey : Parsed<Axdr.Optional<Axdr.OctetString>>
        ResponseAllowed : Parsed<Axdr.Default<Axdr.Boolean>>
        ProposedQualityOfService : Parsed<Axdr.Optional<Axdr.Integer8>>
        ProposedDlmsVersionNumber : Parsed<Axdr.Unsigned8>
        ProposedConformance : Parsed<ConformanceRaw>
        ClientMaxReceivePduSize : Parsed<Axdr.Unsigned16>
    }

module InitiateRequestRaw =
    let parseBody : Parser<InitiateRequestRaw> =
        parser {
            let! dedicatedKey =
                parseNode "dedicated-key" <|
                Axdr.Optional.parse Axdr.OctetString.parse

            let! responseAllowed =
                parseNode "response-allowed" <|
                Axdr.Default.parse Axdr.Boolean.parse

            let! proposedQualityOfService =
                parseNode "proposed-quality-of-service" <|
                Axdr.Optional.parse Axdr.Integer8.parse

            let! proposedDlmsVersionNumber =
                parseNode "proposed-dlms-version-number" <|
                Axdr.Unsigned8.parse

            let! proposedConformance =
                ConformanceRaw.parse

            let! clientMaxReceivePduSize =
                parseNode "client-max-receive-pdu-size" <|
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

    let parse : Parser<Parsed<InitiateRequestRaw>> =
        parseNode "initiate-request" <|
            parser {
                do! Tag.expect XdlmsTag.InitiateRequest
                return! parseBody
            }