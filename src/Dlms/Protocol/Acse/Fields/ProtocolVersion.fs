namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type ProtocolVersion =
    | Version1

module ProtocolVersion =

    let private isVersion1 (bits: Ber.BitString) =
        bits.UnusedBitCount = 7uy
        && bits.Payload.Length = 1
        && bits.Payload.Span[0] = 0x80uy

    let validateValue
        (raw: Field<Ber.BitString>)
        : Validation<ProtocolVersion> =

        validator {
            do!
                ensure
                    raw
                    "unsupported protocol-version value"
                    (isVersion1 raw.Value)

            return Version1
        }
