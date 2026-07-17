module Metering.Dlms.Protocol.Xdlms

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Dlms.Protocol.Xdlms

type InitiateRequestRaw =
    {
        DedicatedKey : Field<Axdr.Optional<Axdr.OctetString>>
        ResponseAllowed : Field<Axdr.Default<Axdr.Boolean>>
        ProposedQualityOfService : Field<Axdr.Optional<Axdr.Integer8>>
        ProposedDlmsVersionNumber : Field<Axdr.Unsigned8>
        ProposedConformance : Field<ConformanceRaw>
        ClientMaxReceivePduSize : Field<Axdr.Unsigned16>
    }

module InitiateRequestRaw =
    let parseBody : Parser<InitiateRequestRaw> =
        parser {
            let! dedicatedKey =
                parseField "dedicated-key" <|
                Axdr.Optional.parse Axdr.OctetString.parse

            let! responseAllowed =
                parseField "response-allowed" <|
                Axdr.Default.parse Axdr.Boolean.parse

            let! proposedQualityOfService =
                parseField "proposed-quality-of-service" <|
                Axdr.Optional.parse Axdr.Integer8.parse

            let! proposedDlmsVersionNumber =
                parseField "proposed-dlms-version-number" <|
                Axdr.Unsigned8.parse

            let! proposedConformance =
                parseField "proposed-conformance" <|
                ConformanceRaw.parse

            let! clientMaxReceivePduSize =
                parseField "client-max-receive-pdu-size" <|
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

    let parse : Parser<Field<InitiateRequestRaw>> =
        parseField "initiate-request" <|
            parser {
                do! Tag.expect XdlmsTag.InitiateRequest
                return! parseBody
            }