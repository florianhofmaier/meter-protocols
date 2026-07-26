namespace Metering.Mbus.Protocol.Frames.NetworkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Mbus.Protocol.Frames

type NwlRaw =
    private Nwl of Unit

module NwlRaw =

    let tryParse : Parser<Field<NwlRaw> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | CiField.Nwl ->
                return!
                    fail "Network Layer (NWL) is not supported"

            | _ ->
                return None
        }