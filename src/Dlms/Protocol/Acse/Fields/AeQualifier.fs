namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type AeQualifier =
    private
        AeQualifier of Ber.OctetString

module AeQualifier =

    let create value =
        AeQualifier value

    let value (AeQualifier value) =
        value

    let validate (raw: Field<Ber.OctetString>) =
        raw.Value |> create |> passed

