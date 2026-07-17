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
        (raw: Field<Axdr.Unsigned16>)
        : Validation<Field<ClientMaxReceivePduSize>> =

        raw.Value
        |> create
        |> fun value -> Field.withValue value raw
        |> passed
