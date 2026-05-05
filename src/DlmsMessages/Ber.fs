module DlmsMessages.Ber

open System
open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core
open DlmsMessages.Utility

type UniversalTag =
    | Integer = 0x02uy
    | OctetString = 0x04uy
    | ObjectIdentifier = 0x06uy

let private runParserAllowNoRemainder (p: Parser<'a>) =
    parser {
        let! value = p
        let! remaining = remainder
        if remaining <> 0 then
            return! fail $"BER content not fully consumed: {remaining} byte(s) remaining"
        else
            return value
    }

let parseLengthDelimited (p: Parser<'a>) : Parser<'a> =
    parser {
        let! len = parseLength
        return! runOnSubSlice len (runParserAllowNoRemainder p)
    }

let parseTagged<'a, 'b when 'a : enum<byte> and 'a: equality> (expectedTag: 'a) (p: Parser<'b>) : Parser<'b> =
    parser {
        do! Tag.expect expectedTag
        return! parseLengthDelimited p
    }

let parseTaggedOptional<'a, 'b when 'a : enum<byte> and 'a: equality> expectedTag (p: Parser<'b>) =
    parser {
        let! tag = Tag.tryParseExact<'a> expectedTag
        match tag with
        | Some _ ->
            return! parseLengthDelimited p |>> Some
        | None ->
            return None
    }

type OctetString = private OctetString of BufferSlice

module OctetString =
    let fromBufferSlice bufferSlice =
        OctetString bufferSlice

    let toBytes (OctetString bytes) =
        BufferSlice.slice bytes

    let toBufferSlice (OctetString bufferSlice) =
        bufferSlice

    let parseContent len: Parser<OctetString> =
        parser {
            let! start = pos
            let! bytes = getBuffer
            return fromBufferSlice <| BufferSlice.create bytes start len
        }

    let parse: Parser<OctetString> =
        parser {
            do! Tag.expect UniversalTag.OctetString
            let! len = parseLength
            return! parseContent len
        }

module Integer =
    let private parseIntegerBytes : Parser<ReadOnlyMemory<byte>> =
        parseTagged UniversalTag.Integer takeAllMem

    let parseUint32 : Parser<uint32> =
        parser {
            let! bytes = parseIntegerBytes

            if bytes.Length = 0 then
                return! fail "INTEGER must not be empty"

            if (bytes.Span[0] &&& 0x80uy) <> 0uy then
                return! fail "negative INTEGER not supported here"

            let value = decodeBigEndianUint64 bytes

            if value > uint64 UInt32.MaxValue then
                return! fail $"INTEGER too large for uint32: {value}"
            else
                return uint32 value
        }

type BitString =
    {
        UnusedBitCount : byte
        Payload : ReadOnlyMemory<byte>
    }

module BitString =
    let parseContent: Parser<BitString> =
        parser {
            let! len = remainder
            if len < 1 then
                return! fail "BIT STRING content must contain at least the unused-bits octet"
            else
                let! unusedBitCount = parseU8

                if unusedBitCount > 7uy then
                    return! fail $"invalid BIT STRING unused bit count {unusedBitCount}"
                else
                    let! payload = takeMem (len - 1)
                    return { UnusedBitCount = unusedBitCount; Payload = payload }
        }

    let parseImplicit: Parser<BitString> =
        parseLengthDelimited parseContent

type ObjectIdentifier = private ObjectIdentifier of ReadOnlyMemory<byte>

module ObjectIdentifier =

    let fromBytes bytes =
        ObjectIdentifier bytes

    let toBytes (ObjectIdentifier bytes) =
        bytes

    let parseContent: Parser<ObjectIdentifier> =
        takeAllMem |>> fromBytes

    let parse: Parser<ObjectIdentifier> =
        parseTagged UniversalTag.ObjectIdentifier parseContent

type GraphicString = private GraphicString of ReadOnlyMemory<byte>

module GraphicString =
    let fromBytes bytes =
        GraphicString bytes

    let toBytes (GraphicString bytes) =
        bytes

    let parseContent: Parser<GraphicString> =
        takeAllMem |>> fromBytes

    let parseImplicit: Parser<GraphicString> =
        parseLengthDelimited parseContent