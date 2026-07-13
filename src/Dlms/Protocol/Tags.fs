module Metering.Dlms.Protocol.Tag

open System
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility


let tryPeek<'a when 'a : enum<byte> and 'a : equality> : Parser<'a option> =
    parser {
        let! tag = peekU8
        if Enum.IsDefined(typeof<'a>, tag) then
            return Some (LanguagePrimitives.EnumOfValue<byte, 'a> tag)
        else
            return None
    }

let tryParseExact<'a when 'a : enum<byte> and 'a : equality> expectedTag : Parser<'a option> =
    parser {
        let! tag = peekU8
        if Enum.IsDefined(typeof<'a>, tag) then
            let enumValue = LanguagePrimitives.EnumOfValue<byte, 'a> tag
            if enumValue = expectedTag then
                do! skip 1
                return Some enumValue
            else
                return None
        else
            return None
    }

let private validateTag tag =
    parser {
        if Enum.IsDefined(typeof<'a>, tag) then
            return LanguagePrimitives.EnumOfValue<byte, 'a> tag
        else
            return! fail $"invalid enum value for type {typeof<'a>.Name}"
    }

let parse<'a when 'a : enum<byte>> : Parser<'a> =
    parser {
        let! tag = parseU8
        return! validateTag tag
    }

let peek<'a when 'a : enum<byte>> : Parser<'a> =
    parser {
        let! tag = peekU8
        return! validateTag tag
    }

let expect<'a when 'a : enum<byte> and 'a : equality> (expected: 'a) : Parser<unit> =
    parser {
        let! tag = parse<'a>
        if tag <> expected then
            return! fail $"expected tag {expected}, got {tag}"
    }