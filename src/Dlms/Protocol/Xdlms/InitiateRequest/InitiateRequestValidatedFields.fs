namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Xdlms

type InitiateRequestValidatedFields =
    {
        DedicatedKey : Field<DedicatedKey> option
        ResponseAllowed : Field<ResponseAllowed>
        ProposedQualityOfService : Field<ProposedQualityOfService> option
        ProposedDlmsVersionNumber : Field<DlmsVersionNumber>
        ProposedConformance : Field<Conformance>
        ClientMaxReceivePduSize : Field<ClientMaxReceivePduSize>
    }

module InitiateRequestValidatedFields =

    let fromParsed
        (raw: Field<InitiateRequestRaw>)
        : Validation<InitiateRequestValidatedFields> =

        validator {
            let! dedicatedKey =
                raw.Value.DedicatedKey
                |> Axdr.Optional.validate
                    NoPresenceDiagnostic
                    DedicatedKey.validate

            and! responseAllowed =
                raw.Value.ResponseAllowed
                |> Axdr.Default.validateField
                    ResponseAllowed.defaultValue
                    (Axdr.InfoWhenExplicitDefault "response-allowed is explicitly encoded with its DEFAULT value TRUE")
                    (ResponseAllowed.validate >> map Field.value)

            and! proposedQualityOfService =
                raw.Value.ProposedQualityOfService
                |> Axdr.Optional.validate
                    NoPresenceDiagnostic
                    ProposedQualityOfService.validate

            and! proposedDlmsVersionNumber =
                raw.Value.ProposedDlmsVersionNumber
                |> DlmsVersionNumber.validate

            and! proposedConformance =
                raw.Value.ProposedConformance
                |> Conformance.validate

            and! clientMaxReceivePduSize =
                raw.Value.ClientMaxReceivePduSize
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
