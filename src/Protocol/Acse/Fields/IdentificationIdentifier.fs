namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core

type InvocationIdentifier =
    private
        InvocationIdentifier of uint32

module InvocationIdentifier =

    let create value =
        InvocationIdentifier value

    let value (InvocationIdentifier value) =
        value

    let validate value =
        value |> create |> Validation.ok