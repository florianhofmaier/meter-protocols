module Mbus.BaseParsers.Utility

open System
open Mbus.BaseParsers.Core

let parseValidated (p: Parser<'a>) (v: 'a -> bool) (msg: string): Parser<'a> =
    parser {
        let! a = p
        if v a then return a
        else return! fail $"{msg} (got {a})"
    }

let validate (p: Parser<'a>) (v: 'a -> bool) (msg: string)  : Parser<unit> =
    parser {
        let! _ = parseValidated p v msg
        return ()
    }

let expectMem (expected: ReadOnlyMemory<byte>) : Parser<unit> =
    parser {
        let! mem = takeMem expected.Length
        if not (mem.Span.SequenceEqual(expected.Span)) then
            return! fail $"expected sequence {BitConverter.ToString(expected.ToArray())}, got {BitConverter.ToString(mem.ToArray())}"
    }