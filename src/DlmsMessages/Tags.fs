module DlmsMessages.Tag

open System
open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core

let tryParseExact<'a when 'a : enum<byte> and 'a : equality> expectedTag : Parser<'a option> =
    withCtx typeof<'a>.Name <|
    parser {
        let! tag = peekU8
        if Enum.IsDefined(typeof<'a>, tag) then
            let enumValue = LanguagePrimitives.EnumOfValue<byte, 'a> tag
            if enumValue = expectedTag then
                do! skipByte
                return Some enumValue
            else
                return None
        else
            return None
    }

let parse<'a when 'a : enum<byte>> : Parser<'a> =
    withCtx typeof<'a>.Name <|
    parser {
        let! tag = parseU8
        if Enum.IsDefined(typeof<'a>, tag) then
            return LanguagePrimitives.EnumOfValue<byte, 'a> tag
        else
            return! fail $"invalid enum value for type {typeof<'a>.Name}"
    }

let expect<'a when 'a : enum<byte> and 'a : equality> (expected: 'a) : Parser<unit> =
    parser {
        let! tag = parse<'a>
        if tag <> expected then
            return! fail $"expected tag {expected}, got {tag}"
    }