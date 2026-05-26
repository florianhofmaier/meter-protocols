namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type SenderAcseRequirements =
    | AuthenticationNotSelected
    | AuthenticationSelected

module SenderAcseRequirements =

    let private isAuthenticationBitSet (bits: Ber.BitString) =
        bits.Payload.Length > 0
        && (bits.Payload.Span[0] &&& 0x80uy) <> 0uy

    let validate
        (raw: ParsedField<Ber.BitString>)
        : Validation<SenderAcseRequirements> =

        validator {
            if isAuthenticationBitSet raw.Value then
                return AuthenticationSelected

            else
                do!
                    info
                        <| raw
                        <| "sender-acse-requirements is present, but authentication bit is not set; authentication functional unit is not selected"

                return AuthenticationNotSelected
        }