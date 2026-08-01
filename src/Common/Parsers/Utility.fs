module Metering.Common.Decoding.Parsers.Utility

open System
open ErrorHandling
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let private fromReaderResult
    (ctx: ParserContext)
    (result: Result<'a, ReaderError>)
    : 'a =

    match result with
    | Ok value -> value
    | Error error ->
        failWith {
            Source = ctx.Source
            Pos = error.Pos
            Msg = error.Msg
        } ctx

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
    fun ctx ->
        ctx.Reader.Skip(count)
        |> fromReaderResult ctx

let remaining : Parser<int> =
    fun ctx -> ctx.Reader.Remaining

let position : Parser<int> =
    fun ctx -> ctx.Reader.Position

let take (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx ->
        ctx.Reader.Read(count)
        |> fromReaderResult ctx

let peek (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx ->
        ctx.Reader.Peek(count)
        |> fromReaderResult ctx

let takeAll : Parser<ReadOnlyMemory<byte>> =
    remaining >>= take

let bufferSliceAt (start: int) (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx ->
        let bufferAbsoluteStart =
            int64 ctx.Reader.Position
            - (int64 ctx.Reader.Buffer.Length - int64 ctx.Reader.Remaining)

        let localStart =
            int64 start - bufferAbsoluteStart

        let localEnd =
            localStart + int64 count

        if count < 0
           || localStart < 0L
           || localEnd > int64 ctx.Reader.Buffer.Length then

            failWith {
                Source = ctx.Source
                Pos = ctx.Reader.Position
                Msg =
                    $"Cannot slice buffer at absolute offset {start} with length {count}. Current buffer starts at offset {bufferAbsoluteStart} and has length {ctx.Reader.Buffer.Length}."
            } ctx
        else
            ctx.Reader.Buffer.Slice(int localStart, count)

let runOnSubSlice (count: int) (parse: Parser<'a>) : Parser<'a> =
    fun ctx ->
        let subReader =
            ctx.Reader.Slice(count)
            |> fromReaderResult ctx

        let value =
            parse { ctx with Reader = subReader }

        if subReader.Remaining <> 0 then
            failWith {
                Source = ctx.Source
                Pos = subReader.Position
                Msg = $"Sub-slice not fully consumed. {subReader.Remaining} byte(s) remaining."
            } ctx

        ctx.Reader.Skip(count)
        |> fromReaderResult ctx

        value

let parseUntilEnd (p: Parser<'a>) : Parser<'a list> =
    fun ctx ->
        let rec loop acc =
            if ctx.Reader.Remaining = 0 then
                List.rev acc
            else
                let before = ctx.Reader.Position
                let item = p ctx
                let after = ctx.Reader.Position

                if after = before then
                    failWith {
                        Source = ctx.Source
                        Pos = before
                        Msg = "Parser did not consume any input in parseUntilEnd."
                    } ctx
                else
                    loop (item :: acc)

        loop []
