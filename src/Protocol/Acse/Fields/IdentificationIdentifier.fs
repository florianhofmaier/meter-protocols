namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

type InvocationIdentifier =
    private
        InvocationIdentifier of uint32

module InvocationIdentifier =

    let create value =
        InvocationIdentifier value

    let value (InvocationIdentifier value) =
        value

    let validate (raw: ParsedField<uint32>) =
        raw.Value |> create |> passed