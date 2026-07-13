module Metering.Dlms.Protocol.Utility

open System
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility

let decodeBigEndianUint64 (bytes: ReadOnlyMemory<byte>) =
    let mutable value = 0UL
    for b in bytes.Span do
        value <- (value <<< 8) ||| uint64 b
    value

let private parseLongLength firstByte: Parser<int> =
    parser {
        let octetCount = int (firstByte &&& 0x7Fuy)
        match octetCount with
        | 0 ->
            return! fail "indefinite BER length is not supported"
        | n when n > 4 ->
            return! fail $"BER length with {n} octets is not supported"
        | n ->
            let! bytes = take n
            let value = decodeBigEndianUint64 bytes
            if value > uint64 Int32.MaxValue then
                return! fail $"BER length too large for int32: {value}"
            else
                return int value
    }

let parseLength : Parser<int> =
    parser {
        let! first = parseU8
        if first < 0x80uy then
            return int first
        else
            return! parseLongLength first
    }