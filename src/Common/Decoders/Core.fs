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

type DecodeContext =
    {
        CreateReader : ReaderFactory
        Sources : ISourceStore
        Trace : IFieldTracer
    }

type Decoder<'a> =
    DecodeContext -> DecodeResult<'a>

type Peek<'a> =
    Field<ReadOnlyMemory<byte>> -> Result<'a, ParserError>

/// A blocker type for stages which are always attempted.
type Never =
    private Never of unit

/// The complete observable outcome of one structural decode stage.
[<RequireQualifiedAccess>]
type Stage<'raw, 'valid, 'notReached> =
    | Reached of raw: 'raw * validation: Validation<'valid>
    | Malformed of ParserError
    | NotReached of 'notReached

/// The observable outcome of a context-dependent transformation.
[<RequireQualifiedAccess>]
type Transformation<'value, 'failure, 'notAttempted> =
    | Succeeded of 'value
    | Failed of 'failure
    | NotAttempted of 'notAttempted

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

            match ParserRunner.runExactlyWithSource source.Span.Source reader context.Sources context.Trace parser with
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

    /// Decodes a stage by parsing and validating it without turning its
    /// structural or semantic diagnostics into a failure of the surrounding
    /// decode pipeline.
    let decodeStage
        (parser: Parser<'raw>)
        (validator: 'raw -> Validation<'valid>)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Stage<'raw, 'valid, 'notReached>> =

        fun context ->
            match runParser parser source context with
            | Decoded (raw, notices) ->
                Decoded (Stage.Reached (raw, validator raw), notices)

            | DecodeFailed (ParseFailed error, notices) ->
                Decoded (Stage.Malformed error, notices)

            | DecodeFailed (failure, notices) ->
                DecodeFailed (failure, notices)

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
            let source =
                context.Sources.AddDerived
                    name
                    origin.Span
                    transform
                    bytes
                    sensitive

            context.Trace.SourceCreated source

            Decoded (
                {
                    Id = origin.Id
                    Span = {
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

    member _.Delay(factory: unit -> Decoder<'a>) : Decoder<'a> =
        fun context -> factory () context

let decoder =
    DecoderBuilder()
