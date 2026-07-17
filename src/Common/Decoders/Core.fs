module Metering.Common.Decoding.Decoders.Core

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core

type DecodeFailure =
    | ParseFailed of ParserError
    | ValidationFailed of Failures
    | EncryptionFailed of Issue

type DecodeResult<'a> =
    | Decoded of 'a * Notice list
    | DecodeFailed of DecodeFailure * Notice list

type ReaderFactory =
    ReadOnlyMemory<byte> -> int -> IByteReader

type ISourceStore =

    abstract AddRoot :
        name: string ->
        bytes: ReadOnlyMemory<byte> ->
        sensitive: bool ->
        SourceInfo

    abstract AddDerived :
        name: string ->
        origin: SourceSpan ->
        transform: SourceTransform ->
        bytes: ReadOnlyMemory<byte> ->
        sensitive: bool ->
        SourceInfo

    abstract CreateReader :
        source: SourceId -> IByteReader

type DecodeContext =
    {
        CreateReader : ReaderFactory
        Sources : ISourceStore option
        Trace : IFieldTracer
    }

type Decoder<'a> =
    DecodeContext -> DecodeResult<'a>

type Peek<'a> =
    Field<ReadOnlyMemory<byte>> -> Result<'a, ParserError>

module Core =

    let decodePassed value : Decoder<'a> =
        fun _ ->
            Decoded (value, [])

    let decodeError failure : Decoder<'a> =
        fun _ ->
            DecodeFailed (failure, [])

    let private runParser
        (parser: Parser<'a>)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<'a> =

        fun context ->
            let reader =
                context.CreateReader source.Value source.Span.Offset

            match ParserRunner.runWithSource source.Span.Source reader context.Trace parser with
            | Ok value ->
                Decoded (value, [])

            | Error error ->
                DecodeFailed (ParseFailed error, [])

    let parse
        (parser: Parser<Field<'raw>>)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<'raw>> =

        runParser parser source

    let parseValue
        (parser: Parser<'value>)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<'value> =

        runParser parser source

    let validate
        (validator: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw>)
        : Decoder<'valid> =

        fun _ ->
            match validator raw with
            | Passed (valid, notices) ->
                Decoded (valid, notices)

            | Failed (failures, notices) ->
                DecodeFailed (ValidationFailed failures, notices)

    let createDerivedSource
        (name: string)
        (transform: SourceTransform)
        (sensitive: bool)
        (origin: Field<_>)
        (bytes: ReadOnlyMemory<byte>)
        : Decoder<Field<ReadOnlyMemory<byte>>> =

        fun context ->
            match context.Sources with
            | None ->
                let issue =
                    {
                        FieldId = origin.Id
                        Message = "decode source store is required to register a derived source"
                    }

                DecodeFailed (EncryptionFailed issue, [])

            | Some sources ->
                let source =
                    sources.AddDerived
                        name
                        origin.Span
                        transform
                        bytes
                        sensitive

                context.Trace.SourceCreated source

                Decoded (
                    {
                        Id = origin.Id
                        Span =
                            {
                                Source = source.Id
                                Offset = 0
                                Length = bytes.Length
                            }
                        Value = bytes
                    },
                    []
                )

type DecoderBuilder() =

    member _.Return(value: 'a) : Decoder<'a> =
        Core.decodePassed value

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
