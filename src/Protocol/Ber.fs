module Metering.Dlms.Protocol.Ber

open System

open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility
open Metering.Dlms.Protocol.Utility

type UniversalTag =
    | Integer = 0x02uy
    | OctetString = 0x04uy
    | ObjectIdentifier = 0x06uy

let parseLengthDelimited (p: Parser<'a>) : Parser<'a> =
    parser {
        let! len = parseLength
        return! runOnSubSlice len p
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

type OctetString =
    private
        OctetString of ReadOnlyMemory<byte>

module OctetString =

    let toBytes (OctetString bytes) =
        bytes

    let length (OctetString bytes) =
        bytes.Length

    let parse: Parser<OctetString> =
        parser {
            do! Tag.expect UniversalTag.OctetString
            let! len = parseLength
            return! take len |>> OctetString
        }

    // let parseWith p : Parser<'a> =
    //     parser {
    //         do! Tag.expect UniversalTag.OctetString
    //         let! len = parseLength
    //         return! runOnSubSlice len p
    //     }

module Integer =

    let private parseIntegerBytes : Parser<ReadOnlyMemory<byte>> =
        parseTagged UniversalTag.Integer takeAll

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
            let! len = remaining
            if len < 1 then
                return! fail "BIT STRING content must contain at least the unused-bits octet"
            else
                let! unusedBitCount = parseU8

                if unusedBitCount > 7uy then
                    return! fail $"invalid BIT STRING unused bit count {unusedBitCount}"
                else
                    let! payload = take (len - 1)
                    return { UnusedBitCount = unusedBitCount; Payload = payload }
        }

    let parseImplicit: Parser<BitString> =
        parseLengthDelimited parseContent

type ObjectIdentifier =
    private
        ObjectIdentifier of ReadOnlyMemory<byte>

module ObjectIdentifier =

    let fromBytes bytes =
        ObjectIdentifier bytes

    let toBytes (ObjectIdentifier bytes) =
        bytes

    let parseContent: Parser<ObjectIdentifier> =
        takeAll |>> fromBytes

    let parse: Parser<ObjectIdentifier> =
        parseTagged UniversalTag.ObjectIdentifier parseContent

type GraphicString =
    private
        GraphicString of ReadOnlyMemory<byte>

module GraphicString =

    let fromBytes bytes =
        GraphicString bytes

    let toBytes (GraphicString bytes) =
        bytes

    let parseContent: Parser<GraphicString> =
        takeAll |>> fromBytes

    let parseImplicit: Parser<GraphicString> =
        parseLengthDelimited parseContent