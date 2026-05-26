namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type ProposedQualityOfService =
    private ProposedQualityOfService of Axdr.Integer8

module ProposedQualityOfService =

    let create value =
        ProposedQualityOfService value

    let value (ProposedQualityOfService value) =
        value

    let validate
        (value: Axdr.Integer8)
        : Validation<ProposedQualityOfService> =

        value |> create |> Validation.ok