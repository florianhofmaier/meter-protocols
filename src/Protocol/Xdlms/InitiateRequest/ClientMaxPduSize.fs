namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type ClientMaxReceivePduSize =
    private
        ClientMaxReceivePduSize of Axdr.Unsigned16

module ClientMaxReceivePduSize =

    let create value =
        ClientMaxReceivePduSize value

    let value (ClientMaxReceivePduSize value) =
        value

    let validate
        (raw: Axdr.Unsigned16)
        : Validation<ClientMaxReceivePduSize> =

        raw |> create |> Validation.ok