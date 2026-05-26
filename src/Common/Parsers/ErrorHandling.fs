module Metering.Common.Decoding.Parsers.ErrorHandling

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types

let fail<'a> (message: string) : Parser<'a> =
    fun ctx ->
        raise (
            ParserException
                {
                    Msg = message
                    Pos = ctx.Reader.Position
                }
        )