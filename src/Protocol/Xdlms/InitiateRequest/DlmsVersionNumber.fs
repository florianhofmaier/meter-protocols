namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type DlmsVersionNumber =
    | Version6

module DlmsVersionNumber =

    let validate
        (raw: ParsedField<Axdr.Unsigned8>)
        : Validation<DlmsVersionNumber> =

        if Axdr.Unsigned8.value raw.Value = 6uy then
            passed Version6

        else
            failed
            <| raw
            <| "DLMS version number must be 6"