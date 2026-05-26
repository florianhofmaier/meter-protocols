namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type AeQualifier =
    private
        AeQualifier of Ber.OctetString

module AeQualifier =

    let create value =
        AeQualifier value

    let value (AeQualifier value) =
        value

    let validate value =
        value |> create |> Validation.ok

