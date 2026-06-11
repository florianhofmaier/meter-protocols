module Metering.Mbus.Protocol.Utility.BcdParsers

open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling

let parseBcd2Digit: Parser<uint8> =
    parser {
        let! b = parseU8
        let lowNibble = b &&& 0x0Fuy
        let highNibble = b >>> 4
        if highNibble <= 9uy && lowNibble <= 9uy then
            return highNibble * 10uy + lowNibble
        else
            return! failBefore 1 $"invalid BCD byte: 0x{b:X2}"
    }

let parseBcd4Digit: Parser<uint16> =
    parser {
        let! b0 = parseBcd2Digit
        let! b1 = parseBcd2Digit
        return uint16 b0 + (uint16 b1 * 100us)
    }

let parseBcd6Digit: Parser<uint32> =
    parser {
        let! b0 = parseBcd2Digit
        let! b1 = parseBcd2Digit
        let! b2 = parseBcd2Digit
        return uint32 b0 + (uint32 b1 * 100u) + (uint32 b2 * 10000u)
    }

let parseBcd8Digit : Parser<uint32> =
    parser {
        let! n0 = parseBcd4Digit
        let! n1 = parseBcd4Digit
        return uint32 n0 + (uint32 n1 * 10000u)
    }

let parseBcd12Digit : Parser<uint64> =
    parser {
        let! n0 = parseBcd4Digit
        let! n1 = parseBcd4Digit
        let! n2 = parseBcd4Digit
        return uint64 n0 + (uint64 n1 * 10000UL) + (uint64 n2 * 100000000UL)
    }
