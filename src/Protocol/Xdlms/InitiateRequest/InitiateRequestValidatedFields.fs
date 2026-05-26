namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Common.Validators.Fields
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Xdlms

type InitiateRequestValidatedFields =
    {
        DedicatedKey : DedicatedKey option
        ResponseAllowed : ResponseAllowed
        ProposedQualityOfService : ProposedQualityOfService option
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : Conformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module InitiateRequestValidatedFields =

    let fromRaw
        (raw: InitiateRequestRaw)
        : Validation<InitiateRequestValidatedFields> =

        validator {
            let! dedicatedKey =
                raw.DedicatedKey
                |> Axdr.Optional.validateParsed
                    NoPresenceDiagnostic
                    DedicatedKey.validate

            and! responseAllowed =
                raw.ResponseAllowed
                |> Axdr.Default.validateParsed
                    ResponseAllowed.defaultValue
                    (Axdr.InfoWhenExplicitDefault "response-allowed is explicitly encoded with its DEFAULT value TRUE")
                    ResponseAllowed.validate

            and! proposedQualityOfService =
                raw.ProposedQualityOfService
                |> Axdr.Optional.validateParsed
                    NoPresenceDiagnostic
                    ProposedQualityOfService.validate

            and! proposedDlmsVersionNumber =
                raw.ProposedDlmsVersionNumber
                |> Axdr.Required.validateParsed
                    DlmsVersionNumber.validate

            and! proposedConformance =
                raw.ProposedConformance
                |> Axdr.Required.validateParsed
                    Conformance.validate

            and! clientMaxReceivePduSize =
                raw.ClientMaxReceivePduSize
                |> Axdr.Required.validateParsed
                    ClientMaxReceivePduSize.validate

            return {
                DedicatedKey = dedicatedKey
                ResponseAllowed = responseAllowed
                ProposedQualityOfService = proposedQualityOfService
                ProposedDlmsVersionNumber = proposedDlmsVersionNumber
                ProposedConformance = proposedConformance
                ClientMaxReceivePduSize = clientMaxReceivePduSize
            }
        }