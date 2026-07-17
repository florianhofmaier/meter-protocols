namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type DlmsVersionNumber =
    | Version6

module DlmsVersionNumber =

    let validate
        (raw: Field<Axdr.Unsigned8>)
        : Validation<Field<DlmsVersionNumber>> =

        if Axdr.Unsigned8.value raw.Value = 6uy then
            raw
            |> Field.withValue Version6
            |> passed

        else
            failed
            <| raw
            <| "DLMS version number must be 6"
