module Metering.Common.Decoding.Parsers.Binary

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Parsers.Core
open Utility

let parseU8 : Parser<uint8> =
    _.Reader.Read(1).Span[0]

let peekU8 : Parser<uint8> =
    _.Reader.Peek(1).Span[0]

let expectU8 expected : Parser<unit> =
   expect parseU8 expected

let parseI8 : Parser<int8> =
    _.Reader.Read(1).Span[0] |>> int8

let parseU16LittleEndian : Parser<uint16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadUInt16LittleEndian bytes.Span
    }

let parseU16BigEndian : Parser<uint16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadUInt16BigEndian bytes.Span
    }

let expectU16BigEndian expected : Parser<unit> =
    expect parseU16BigEndian expected

let parseI16LittleEndian : Parser<int16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadInt16LittleEndian bytes.Span
    }

let parseI16BigEndian : Parser<int16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadInt16BigEndian bytes.Span
    }

let parseU24LittleEndian: Parser<uint32> =
    parser {
        let! bytes = take 3

        let value =
            uint32 bytes.Span[0] <<< 16
            ||| uint32 bytes.Span[1] <<< 8
            ||| uint32 bytes.Span[2]

        return value
    }

let parseI24LittleEndian: Parser<int32> =
    parseU24LittleEndian
    |>> (fun u ->
        if (u &&& (1u <<< 23)) <> 0u then
            int32 (u - (1u <<< 24))
        else
            int32 u
    )

let parseU32LittleEndian : Parser<uint32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadUInt32LittleEndian bytes.Span
    }

let parseU32BigEndian : Parser<uint32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadUInt32BigEndian bytes.Span
    }

let parseI32LittleEndian : Parser<int32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadInt32LittleEndian bytes.Span
    }

let parseI32BigEndian : Parser<int32> =
    parser {
        let! bytes = take 4
        return BinaryPrimitives.ReadInt32BigEndian bytes.Span
    }

let parseU48LittleEndian : Parser<uint64> =
    parser {
        let! bytes = take 6

        let value =
            uint64 bytes.Span[0] <<< 40
            ||| uint64 bytes.Span[1] <<< 32
            ||| uint64 bytes.Span[2] <<< 24
            ||| uint64 bytes.Span[3] <<< 16
            ||| uint64 bytes.Span[4] <<< 8
            ||| uint64 bytes.Span[5]

        return value
    }

let parseI48LittleEndian : Parser<int64> =
    parseU48LittleEndian
    |>> (fun u ->
        if (u &&& (1UL <<< 47)) <> 0UL then
            int64 (u - (1UL <<< 48))
        else
            int64 u
    )

let parseU64LittleEndian : Parser<uint64> =
    parser {
        let! bytes = take 8
        return BinaryPrimitives.ReadUInt64LittleEndian bytes.Span
    }

let parseI64LittleEndian : Parser<int64> =
    parser {
        let! bytes = take 8
        return BinaryPrimitives.ReadInt64LittleEndian bytes.Span
    }