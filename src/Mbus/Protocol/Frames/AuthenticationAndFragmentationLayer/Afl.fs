namespace Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Mbus.Protocol.Frames

type AflRaw = Unit

module AflRaw =

    let tryParse : Parser<Field<AflRaw> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | CiField.Afl ->
                return!
                    fail
                        "Unsupported but standard-conformant AFL variant; the supported path is a complete TPL message. Actual CI=0x90. EN 13757-7:2018, 5.2, Table 2."

            | _ ->
                return None
        }

type Afl = Unit