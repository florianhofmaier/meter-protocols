namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type ImplementationInformation =
    private
        ImplementationInformation of Ber.GraphicString

module ImplementationInformation =

    let create value =
        ImplementationInformation value

    let value (ImplementationInformation value) =
        value

    let validate value =
        value |> create |> Validation.ok