module Metering.Common.Decoding.Parsers.Utility

open System
open ErrorHandling
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let expect<'a when 'a : equality>
    (parse: Parser<'a>)
    (expected: 'a) : Parser<unit> =

    parser {
        let! value = parse
        if value <> expected then
            return! fail $"expected {expected}, but got {value}"
        else
            return ()
    }

let skip (count: int) : Parser<unit> =
    _.Reader.Skip(count)

let remaining : Parser<int> =
    _.Reader.Remaining

let take (count: int) : Parser<ReadOnlyMemory<byte>> =
    _.Reader.Read(count)

let takeAll : Parser<ReadOnlyMemory<byte>> =
    remaining >>= take

let runOnSubSlice (count: int) (parse: Parser<'a>) : Parser<'a> =
    fun ctx ->
        let subReader =
            ctx.Reader.Slice(count)

        let result =
            parse { ctx with Reader = subReader }

        if subReader.Remaining <> 0 then
            raise (
                ParserException
                    {
                        Pos = subReader.Position
                        Msg = $"Sub-slice not fully consumed. {subReader.Remaining} byte(s) remaining."
                    }
            )

        ctx.Reader.Skip(count)
        result

