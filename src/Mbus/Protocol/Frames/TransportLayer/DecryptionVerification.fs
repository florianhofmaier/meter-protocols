namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type DecryptionVerificationRaw =
    {
        Byte1: byte
        Byte2: byte
    }

module DecryptionVerificationRaw =

    let parse : Parser<Field<DecryptionVerificationRaw>> =
        parseField "Decryption Verification"
            <| parser {
                let! b1 = parseU8
                let! b2 = parseU8

                return {
                    Byte1 = b1
                    Byte2 = b2
                }
            }