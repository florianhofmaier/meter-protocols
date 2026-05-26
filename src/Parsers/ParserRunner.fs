namespace Metering.Common.Parsers

open System
open Metering.Common.Parsers.BaseParsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree

module ParserRunner =

    let runFromState
        (p: Parser<'a>)
        (state: ParserState)
        : Result<'a * ParserState, ParserError> =
        p state

    let runOnBytes
        (p: Parser<'a>)
        (bytes: ReadOnlyMemory<byte>)
        : Result<'a, ParserError> =

        let state =
            ParserState.init bytes "root"

        match p state with
        | Ok (value, _) ->
            Ok value

        | Error error ->
            Error error

    let runExact (p : Parser<'a>) : Parser<'a> =
        parser {
            let! value = p
            do! expectEnd
            return value
        }

    let runOnParsedMemory
        (p: Parser<'a>)
        (source: Parsed<ReadOnlyMemory<byte>>)
        : Result<'a, ParserError> =

        let state =
            ParserState.fromParsedMemory source

        match runExact p state with
        | Ok (value, _) ->
            Ok value

        | Error error ->
            Error error