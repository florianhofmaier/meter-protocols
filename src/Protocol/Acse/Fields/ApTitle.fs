namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type ApTitle =
    private
        ApTitle of Ber.OctetString

module ApTitle =

    let create value =
        ApTitle value

    let value (ApTitle value) =
        value

    let validate value =
        value |> create |> Validation.ok