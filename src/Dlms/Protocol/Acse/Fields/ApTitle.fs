namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type ApTitle =
    private
        ApTitle of Ber.OctetString

module ApTitle =

    let create value =
        ApTitle value

    let value (ApTitle value) =
        value

    let validate (raw: Field<Ber.OctetString>) =
        raw.Value |> create |> passed