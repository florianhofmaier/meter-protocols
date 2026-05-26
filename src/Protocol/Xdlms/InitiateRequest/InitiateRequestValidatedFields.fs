namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Validators.Core
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
                |> Axdr.Optional.validate
                    Axdr.NoPresenceDiagnostic
                    DedicatedKey.validate

            and! responseAllowed =
                raw.ResponseAllowed
                |> Axdr.Default.validate
                    ResponseAllowed.defaultValue
                    (Axdr.InfoWhenExplicitDefault "response-allowed is explicitly encoded with its DEFAULT value TRUE")
                    ResponseAllowed.validate

            and! proposedQualityOfService =
                raw.ProposedQualityOfService
                |> Axdr.Optional.validate
                    Axdr.NoPresenceDiagnostic
                    ProposedQualityOfService.validate

            and! proposedDlmsVersionNumber =
                raw.ProposedDlmsVersionNumber
                |> DlmsVersionNumber.validate

            and! proposedConformance =
                raw.ProposedConformance
                |> Conformance.validate

            and! clientMaxReceivePduSize =
                raw.ClientMaxReceivePduSize
                |> ClientMaxReceivePduSize.validate

            return {
                DedicatedKey = dedicatedKey
                ResponseAllowed = responseAllowed
                ProposedQualityOfService = proposedQualityOfService
                ProposedDlmsVersionNumber = proposedDlmsVersionNumber
                ProposedConformance = proposedConformance
                ClientMaxReceivePduSize = clientMaxReceivePduSize
            }
        }