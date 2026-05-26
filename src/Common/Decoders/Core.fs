module Metering.Common.Decoding.Decoders.Core

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography

type DecodeFailure =
    | ParseFailed of ParserError
    | ValidationFailed of Failures
    | EncryptionFailed of EncryptionError

type DecodeResult<'a> =
    | Decoded of 'a * Notice list
    | DecodeFailed of DecodeFailure * Notice list

type DecodeContext =
    {
        CreateReader : ParsedField<ReadOnlyMemory<byte>> -> IByteReader
        Trace : IFieldTracer
    }

type Decoder<'a> =
    DecodeContext -> DecodeResult<'a>

module Core =

    let passed value : Decoder<'a> =
        fun _ ->
            Decoded (value, [])

    let failed failure : Decoder<'a> =
        fun _ ->
            DecodeFailed (failure, [])

    let parse
        (parser: Parser<ParsedField<'raw>>)
        (source: ParsedField<ReadOnlyMemory<byte>>)
        : Decoder<ParsedField<'raw>> =

        fun context ->
            let reader =
                context.CreateReader source

            let parseResult =
                ParserRunner.run reader context.Trace parser

            match parseResult with
            | Ok raw ->
                Decoded (raw, [])

            | Error error ->
                DecodeFailed (ParseFailed error, [])

    let validate
        (validator: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw>)
        : Decoder<'valid> =

        fun _ ->
            match validator raw with
            | Passed (valid, notices) ->
                Decoded (valid, notices)

            | Failed (failures, notices) ->
                DecodeFailed (ValidationFailed failures, notices)

type DecoderBuilder() =

    member _.Return(value: 'a) : Decoder<'a> =
        Core.passed value

    member _.ReturnFrom(decoder: Decoder<'a>) : Decoder<'a> =
        decoder

    member _.Bind
        (
            decoder: Decoder<'a>,
            next: 'a -> Decoder<'b>
        ) : Decoder<'b> =

        fun context ->
            match decoder context with
            | Decoded (value, notices1) ->
                match next value context with
                | Decoded (nextValue, notices2) ->
                    Decoded (nextValue, notices1 @ notices2)

                | DecodeFailed (failure, notices2) ->
                    DecodeFailed (failure, notices1 @ notices2)

            | DecodeFailed (failure, notices) ->
                DecodeFailed (failure, notices)

let decoder =
    DecoderBuilder()