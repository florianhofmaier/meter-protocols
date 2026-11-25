module Metering.Common.Decoding.Parsers.ErrorHandling

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

exception private ParserFailure of ParserError

/// <summary>Runs a parser and converts explicit parser failures to Error results; other exceptions propagate.</summary>
let execute (parse: Parser<'a>) (ctx: ParserContext) : Result<'a, ParserError> =

    try
        Ok(parse ctx)
    with ParserFailure error ->
        Error error

/// <summary>Creates a parser that raises an explicit parser failure with the supplied error.</summary>
// Keep the private exception constructor in this assembly when consumers optimize Release builds.
[<NoCompilerInlining>]
let failWith<'a> (error: ParserError) : Parser<'a> = fun _ -> raise (ParserFailure error)

/// <summary>Creates a parser that fails with the supplied message at the current source and reader position.</summary>
let fail<'a> message : Parser<'a> =
    fun ctx ->
        failWith
            {
                Source = ctx.Source
                Msg = message
                Pos = ctx.Reader.Position
            }
            ctx

/// <summary>Creates a parser that reports failure the supplied number of bytes before the current reader position, without rewinding the reader.</summary>
let failBefore<'a> n message : Parser<'a> =
    fun ctx ->
        failWith
            {
                Source = ctx.Source
                Msg = message
                Pos = ctx.Reader.Position - n
            }
            ctx
