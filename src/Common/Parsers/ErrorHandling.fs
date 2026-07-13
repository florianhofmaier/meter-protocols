module Metering.Common.Decoding.Parsers.ErrorHandling

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let fail<'a> message : Parser<'a> =
    fun ctx ->
        raise (
            ParserException
                {
                    Source = ctx.Source
                    Msg = message
                    Pos = ctx.Reader.Position
                }
        )

let failBefore<'a> n message : Parser<'a> =
    fun ctx ->
        raise (
            ParserException
                {
                    Source = ctx.Source
                    Msg = message
                    Pos = ctx.Reader.Position - n
                }
        )
