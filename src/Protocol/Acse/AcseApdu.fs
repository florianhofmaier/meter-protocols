namespace Metering.Dlms.Protocol.Acse

open Metering.Common.Decoding.Parsers
open Metering.Dlms.Protocol.Acse.Aarq

type AcseApduRaw =
    | Aarq of ParsedField<AarqRaw>
    | Aare of AareRaw
    | Rlrq of RlrqRaw
    | Rlre of RlreRaw

module AcseApduRaw =

    let decode =
        decoder {
            let! tag = Tag.peek<AcseTag>

            match tag with
            | AcseTag.Aarq ->
                return! Aarq.decode |>> Aarq

            | AcseTag.Aare ->
                return! fail "AARE not supported"

            | AcseTag.Rlrq ->
                return! fail "RLRQ not supported"

            | AcseTag.Rlre ->
                return! fail "RLRE not supported"

            | _ ->
                return! fail "unknown APDU tag"
        }