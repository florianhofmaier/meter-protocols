module Metering.Common.Decoding.Parsers.ErrorHandling

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

exception private ParserFailure of ParserError

let execute
    (parse: Parser<'a>)
    (ctx: ParserContext)
    : Result<'a, ParserError> =

    try
        Ok (parse ctx)
    with
    | ParserFailure error -> Error error

let failWith<'a> (error: ParserError) : Parser<'a> =
    fun _ -> raise (ParserFailure error)

let fail<'a> message : Parser<'a> =
    fun ctx ->
        failWith {
            Source = ctx.Source
            Msg = message
            Pos = ctx.Reader.Position
        } ctx

let failBefore<'a> n message : Parser<'a> =
    fun ctx ->
        failWith {
            Source = ctx.Source
            Msg = message
            Pos = ctx.Reader.Position - n
        } ctx
