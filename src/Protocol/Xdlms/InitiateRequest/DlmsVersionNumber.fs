namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type DlmsVersionNumber =
    | Version6

module DlmsVersionNumber =

    let validate
        (raw: Axdr.Unsigned8)
        : Validation<DlmsVersionNumber> =

        if Axdr.Unsigned8.value raw = 6uy then
            Validation.ok Version6

        else
            Validation.error "DLMS version number must be 6"