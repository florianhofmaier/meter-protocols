module Metering.Common.Decoding.Parsers.Binary

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

let parseU16BigEndian : Parser<uint16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadUInt16BigEndian bytes.Span
    }

let expectU16BigEndian expected : Parser<unit> =
    expect parseU16BigEndian expected

let parseI16BigEndian : Parser<int16> =
    parser {
        let! bytes = take 2
        return BinaryPrimitives.ReadInt16BigEndian bytes.Span
    }