module Metering.Common.Parsers.Utility

open System
open Metering.Common.Parsers.BaseParsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ErrorHandling

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

let expectEnd : Parser<unit> =
    fun st ->
        if st.Off = st.Buf.Length then ok () st
        else err st "expected end of buffer"

let parseUntilEnd (p: Parser<'a>) : Parser<'a list> =
    fun st ->
        let rec loop st acc =
             if st.Off >= st.Buf.Length then
                 ok (List.rev acc) st
             else
                 let before = st.Off

                 match p st with
                 | Ok (x, stNext) ->
                     if stNext.Off = before then
                         err stNext "parser made no progress in parseUntilEnd"
                     else
                         loop stNext (x::acc)

                 | Error e ->
                     Error e

        loop st []