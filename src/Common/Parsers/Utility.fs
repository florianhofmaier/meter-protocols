module Metering.Common.Decoding.Parsers.Utility

open System
open ErrorHandling
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let private fromReaderResult (ctx: ParserContext) (result: Result<'a, ReaderError>) : 'a =

    match result with
    | Ok value -> value
    | Error error ->
        failWith
            {
                Source = ctx.Source
                Pos = error.Pos
                Msg = error.Msg
            }
            ctx

/// <summary>Runs a parser and requires its result to equal the expected value; a mismatch fails at the resulting reader position.</summary>
let expect<'a when 'a: equality> (parse: Parser<'a>) (expected: 'a) : Parser<unit> =

    parser {
        let! value = parse

        if value <> expected then
            return! fail $"expected {expected}, but got {value}"
        else
            return ()
    }

/// <summary>Skips the requested number of bytes, converting reader errors into parser failures.</summary>
let skip (count: int) : Parser<unit> =
    fun ctx -> ctx.Reader.Skip(count) |> fromReaderResult ctx

/// <summary>Returns the number of unread bytes without advancing the reader.</summary>
let remaining: Parser<int> = fun ctx -> ctx.Reader.Remaining

/// <summary>Returns the reader's current absolute byte position without advancing it.</summary>
let position: Parser<int> = fun ctx -> ctx.Reader.Position

/// <summary>Reads and consumes the requested bytes, converting reader errors into parser failures.</summary>
let take (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx -> ctx.Reader.Read(count) |> fromReaderResult ctx

/// <summary>Returns the requested upcoming bytes without consuming them, converting reader errors into parser failures.</summary>
let peek (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx -> ctx.Reader.Peek(count) |> fromReaderResult ctx

/// <summary>Reads and consumes all bytes remaining in the current reader.</summary>
let takeAll: Parser<ReadOnlyMemory<byte>> = remaining >>= take

/// <summary>Returns a view of the reader buffer at an absolute offset without advancing; fails when the requested range lies outside the buffer.</summary>
let bufferSliceAt (start: int) (count: int) : Parser<ReadOnlyMemory<byte>> =
    fun ctx ->
        let bufferAbsoluteStart =
            int64 ctx.Reader.Position
            - (int64 ctx.Reader.Buffer.Length - int64 ctx.Reader.Remaining)

        let localStart = int64 start - bufferAbsoluteStart

        let localEnd = localStart + int64 count

        if count < 0 || localStart < 0L || localEnd > int64 ctx.Reader.Buffer.Length then

            failWith
                {
                    Source = ctx.Source
                    Pos = ctx.Reader.Position
                    Msg =
                        $"Cannot slice buffer at absolute offset {start} with length {count}. Current buffer starts at offset {bufferAbsoluteStart} and has length {ctx.Reader.Buffer.Length}."
                }
                ctx
        else
            ctx.Reader.Buffer.Slice(int localStart, count)

/// <summary>Requires a parser to consume a bounded upcoming slice exactly, advancing the outer reader only after success.</summary>
let runOnSubSlice (count: int) (parse: Parser<'a>) : Parser<'a> =
    fun ctx ->
        let subReader = ctx.Reader.Slice(count) |> fromReaderResult ctx

        let value = parse { ctx with Reader = subReader }

        if subReader.Remaining <> 0 then
            failWith
                {
                    Source = ctx.Source
                    Pos = subReader.Position
                    Msg = $"Sub-slice not fully consumed. {subReader.Remaining} byte(s) remaining."
                }
                ctx

        ctx.Reader.Skip(count) |> fromReaderResult ctx

        value

/// <summary>Collects parser results until no input remains; fails if an invocation consumes no input.</summary>
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
                    failWith
                        {
                            Source = ctx.Source
                            Pos = before
                            Msg = "Parser did not consume any input in parseUntilEnd."
                        }
                        ctx
                else
                    loop (item :: acc)

        loop []
