module Metering.Common.Parsers.BinaryParsers

open System.Buffers.Binary
open Metering.Common.Parsers.BaseParsers
open Metering.Common.Parsers.Core


let parseU8 : Parser<uint8> = parseByte

let peekU8 : Parser<uint8> = peekByte

let parseI8 : Parser<int8> =
    parseU8 |>> int8

let expectU8 b msg : Parser<unit> =
    parser {
        let! v = parseU8
        if v = b then
            return ()
        else
            return! failBefore 1 $"{msg}: expect 0x{b:X2}, got 0x{v:X2}"
    }

let parseU16LittleEndian: Parser<uint16> =
    parser {
        let! b = takeMem 2
        return BinaryPrimitives.ReadUInt16LittleEndian b.Span
    }

let parseU16BigEndian: Parser<uint16> =
    parser {
        let! b = takeMem 2
        return BinaryPrimitives.ReadUInt16BigEndian b.Span
    }

let expectU16BigEndian expect : Parser<unit> =
    parser {
        let! v = parseU16BigEndian
        if v = expect then
            return ()
        else
            return! failBefore 2 $"expect 0x{expect:X4}, got 0x{v:X4}"
    }

let parseI16LittleEndian: Parser<int16> =
    parseU16LittleEndian |>> int16

let parseI16BigEndian: Parser<int16> =
    parseU16BigEndian |>> int16

let parseU24: Parser<uint32> =
    parser {
        let! b0 = parseU16LittleEndian
        let! b1 = parseU8
        return uint32 b0 ||| (uint32 b1 <<< 16)
    }

let parseI24: Parser<int32> =
    parseU24
    |>> (fun u ->
        if (u &&& (1u <<< 23)) <> 0u then
            int32 (u - (1u <<< 24))
        else
            int32 u
    )

let parseU32: Parser<uint32> =
    parser {
        let! b = takeMem 4
        return BinaryPrimitives.ReadUInt32LittleEndian b.Span
    }

let parseI32: Parser<int32> =
    parseU32 |>> int32

let parseU48: Parser<uint64> =
    parser {
        let! low = parseU32
        let! high = parseU16LittleEndian
        return uint64 low ||| (uint64 high <<< 32)
    }

let parseI48: Parser<int64> =
    parseU48
    |>> (fun u ->
        if (u &&& (1UL <<< 47)) <> 0UL then
            int64 (u - (1UL <<< 48))
        else
            int64 u
    )

let parseU64LittleEndian: Parser<uint64> =
    parser {
        let! b = takeMem 8
        return BinaryPrimitives.ReadUInt64LittleEndian b.Span
    }

let parseU64BigEndian: Parser<uint64> =
    parser {
        let! b = takeMem 8
        return BinaryPrimitives.ReadUInt64BigEndian b.Span
    }

let parseI64: Parser<int64> =
    parseU64LittleEndian |>> int64
