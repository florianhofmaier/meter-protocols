namespace Metering.Dlms.Protocol.Acse

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Dlms.Protocol

type AcseApdu =
    | Aarq of AarqApdu
    | Aare
    | Rlrq
    | Rlre

module AcseApdu =

    let decode
        (bytes: Field<ReadOnlyMemory<byte>>)=
        decoder {
            let! tag = parseValue Tag.peek<AcseTag> bytes

            match tag with
            | AcseTag.Aarq ->
                return! Aarq.decode |>> Aarq

            | AcseTag.Aare ->
                return! fail "AARE not supported" |> Aare

            | AcseTag.Rlrq ->
                return! fail "RLRQ not supported"

            | AcseTag.Rlre ->
                return! fail "RLRE not supported"

            | _ ->
                return! fail "unknown APDU tag"
        }