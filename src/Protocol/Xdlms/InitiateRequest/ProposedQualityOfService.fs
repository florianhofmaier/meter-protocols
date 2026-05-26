namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type ProposedQualityOfService =
    private ProposedQualityOfService of Axdr.Integer8

module ProposedQualityOfService =

    let create value =
        ProposedQualityOfService value

    let value (ProposedQualityOfService value) =
        value

    let validate
        (raw: ParsedField<Axdr.Integer8>)
        : Validation<ProposedQualityOfService> =

        raw.Value |> create |> passed