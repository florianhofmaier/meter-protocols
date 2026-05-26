namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type SenderAcseRequirements =
    | AuthenticationNotSelected
    | AuthenticationSelected

module SenderAcseRequirements =

    let private isAuthenticationBitSet (bits: Ber.BitString) =
        bits.Payload.Length > 0
        && (bits.Payload.Span[0] &&& 0x80uy) <> 0uy

    let validate
        (raw: Ber.BitString)
        : Validation<SenderAcseRequirements> =

        validator {
            if isAuthenticationBitSet raw then
                return AuthenticationSelected

            else
                do!
                    Validation.info
                        "sender-acse-requirements is present, but authentication bit is not set; authentication functional unit is not selected"

                return AuthenticationNotSelected
        }