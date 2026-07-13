namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
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
        (raw: ParsedField<Axdr.Unsigned16>)
        : Validation<ClientMaxReceivePduSize> =

        raw.Value |> create |> passed