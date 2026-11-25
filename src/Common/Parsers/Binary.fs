module Metering.Common.Decoding.Parsers.Binary

open System.Buffers.Binary
open Metering.Common.Decoding.Parsers.Core
open Utility

/// <summary>Reads and consumes a unsigned 8-bit integer.</summary>
let parseU8: Parser<uint8> = take 1 |>> fun bytes -> bytes.Span[0]

/// <summary>Returns the next unsigned 8-bit integer without consuming input.</summary>
let peekU8: Parser<uint8> = peek 1 |>> fun bytes -> bytes.Span[0]

/// <summary>Consumes one byte and fails if it differs from the expected value.</summary>
let expectU8 expected : Parser<unit> = expect parseU8 expected

/// <summary>Reads and consumes a signed 8-bit integer.</summary>
let parseI8: Parser<int8> = parseU8 |>> int8

/// <summary>Reads and consumes a unsigned 16-bit integer in little-endian byte order.</summary>
let parseU16LittleEndian: Parser<uint16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadUInt16LittleEndian bytes.Span
    }

/// <summary>Reads and consumes a unsigned 16-bit integer in big-endian byte order.</summary>
let parseU16BigEndian: Parser<uint16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadUInt16BigEndian bytes.Span
    }

/// <summary>Consumes a big-endian unsigned 16-bit integer and fails if it differs from the expected value.</summary>
let expectU16BigEndian expected : Parser<unit> = expect parseU16BigEndian expected

/// <summary>Reads and consumes a signed 16-bit integer in little-endian byte order.</summary>
let parseI16LittleEndian: Parser<int16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadInt16LittleEndian bytes.Span
    }

/// <summary>Reads and consumes a signed 16-bit integer in big-endian byte order.</summary>
let parseI16BigEndian: Parser<int16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadInt16BigEndian bytes.Span
    }

/// <summary>Reads and consumes a unsigned 24-bit integer in little-endian byte order.</summary>
let parseU24LittleEndian: Parser<uint32> =
    parser {
        let! bytes = take 3

        let value =
            uint32 bytes.Span[0]
            ||| (uint32 bytes.Span[1] <<< 8)
            ||| (uint32 bytes.Span[2] <<< 16)

        return value
    }

/// <summary>Reads and consumes a signed 24-bit integer in little-endian byte order.</summary>
let parseI24LittleEndian: Parser<int32> =
    parseU24LittleEndian
    |>> (fun u ->
        if (u &&& (1u <<< 23)) <> 0u then
            int32 (u - (1u <<< 24))
        else
            int32 u)

/// <summary>Reads and consumes a unsigned 32-bit integer in little-endian byte order.</summary>
let parseU32LittleEndian: Parser<uint32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadUInt32LittleEndian bytes.Span
    }

/// <summary>Reads and consumes a unsigned 32-bit integer in big-endian byte order.</summary>
let parseU32BigEndian: Parser<uint32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadUInt32BigEndian bytes.Span
    }

/// <summary>Reads and consumes a signed 32-bit integer in little-endian byte order.</summary>
let parseI32LittleEndian: Parser<int32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadInt32LittleEndian bytes.Span
    }

/// <summary>Reads and consumes a signed 32-bit integer in big-endian byte order.</summary>
let parseI32BigEndian: Parser<int32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadInt32BigEndian bytes.Span
    }

/// <summary>Reads and consumes a unsigned 48-bit integer in little-endian byte order.</summary>
let parseU48LittleEndian: Parser<uint64> =
    parser {
        let! bytes = take 6

        let value =
            uint64 bytes.Span[0]
            ||| (uint64 bytes.Span[1] <<< 8)
            ||| (uint64 bytes.Span[2] <<< 16)
            ||| (uint64 bytes.Span[3] <<< 24)
            ||| (uint64 bytes.Span[4] <<< 32)
            ||| (uint64 bytes.Span[5] <<< 40)

        return value
    }

/// <summary>Reads and consumes a signed 48-bit integer in little-endian byte order.</summary>
let parseI48LittleEndian: Parser<int64> =
    parseU48LittleEndian
    |>> (fun u ->
        if (u &&& (1UL <<< 47)) <> 0UL then
            int64 (u - (1UL <<< 48))
        else
            int64 u)

/// <summary>Reads and consumes a unsigned 64-bit integer in little-endian byte order.</summary>
let parseU64LittleEndian: Parser<uint64> =
    parser {
        let! bytes = take 8
        return BinaryPrimitives.ReadUInt64LittleEndian bytes.Span
    }

/// <summary>Reads and consumes a signed 64-bit integer in little-endian byte order.</summary>
let parseI64LittleEndian: Parser<int64> =
    parser {
        let! bytes = take 8
        return BinaryPrimitives.ReadInt64LittleEndian bytes.Span
    }
