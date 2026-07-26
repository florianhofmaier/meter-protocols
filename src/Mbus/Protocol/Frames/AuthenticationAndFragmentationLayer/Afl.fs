namespace Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Mbus.Protocol.Frames

type AflRaw =
    private Afl of Unit

module AflRaw =

    let tryParse : Parser<Field<AflRaw> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | CiField.Afl ->
                return!
                    fail "Authentication and fragmentation layer (AFL) is not supported"

            | _ ->
                return None
        }