namespace Metering.Dlms.Protocol.Acse

open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Aarq

type AcseApduRaw =
    | Aarq of AarqRaw
    | Aare of AareRaw
    | Rlrq of RlrqRaw
    | Rlre of RlreRaw

module AcseApduRaw =

    let decode : Parser<Parsed<AcseApduRaw>> =
        parser {
            let! tag = Tag.peek<AcseTag>

            match tag with
            | AcseTag.Aarq ->
                return! AarqRaw.parse |>> Parsed.map Aarq

            | AcseTag.Aare ->
                return! fail "AARE not supported"

            | AcseTag.Rlrq ->
                return! fail "RLRQ not supported"

            | AcseTag.Rlre ->
                return! fail "RLRE not supported"

            | _ ->
                return! fail "unknown APDU tag"
        }