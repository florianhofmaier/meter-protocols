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
        (raw: Field<Axdr.Integer8>)
        : Validation<Field<ProposedQualityOfService>> =

        raw.Value
        |> create
        |> fun value -> Field.withValue value raw
        |> passed
