module Metering.Common.Decoding.Parsers.ParserSource

open System
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Types

let createDerived
    (name: string)
    (transform: SourceTransform)
    (sensitive: bool)
    (origin: Field<_>)
    (bytes: ReadOnlyMemory<byte>)
    : Parser<Field<ReadOnlyMemory<byte>>> =

    fun context ->
        let source =
            context.Sources.AddDerived
                name
                origin.Span
                transform
                bytes
                sensitive

        context.Trace.SourceCreated source

        {
            Id = origin.Id
            Span =
                {
                    Source = source.Id
                    Offset = 0
                    Length = bytes.Length
                }
            Value = bytes
        }

let private nestedReader
    (context: ParserContext)
    (source: Field<ReadOnlyMemory<byte>>)
    : Result<IByteReader, ParserError> =

    let reader =
        context.Sources.CreateReader source.Span.Source

    let skipCount =
        source.Span.Offset - reader.Position

    if skipCount < 0 then
        Error {
            Source = source.Span.Source
            Pos = reader.Position
            Msg =
                $"Cannot create nested parser source at offset {source.Span.Offset}; "
                + $"the source reader starts at offset {reader.Position}."
        }
    else
        reader.Skip skipCount
        |> Result.mapError (fun error ->
            {
                Source = source.Span.Source
                Pos = error.Pos
                Msg = error.Msg
            })
        |> Result.bind (fun () ->
            reader.Slice source.Span.Length
            |> Result.mapError (fun error ->
                {
                    Source = source.Span.Source
                    Pos = error.Pos
                    Msg = error.Msg
                }))

let parseExactly
    (source: Field<ReadOnlyMemory<byte>>)
    (parse: Parser<'a>)
    : Parser<'a> =

    fun context ->
        match nestedReader context source with
        | Error error ->
            failWith error context
        | Ok reader ->
            let nestedContext =
                {
                    context with
                        Reader = reader
                        Source = source.Span.Source
                }

            let value =
                parse nestedContext

            if reader.Remaining <> 0 then
                fail
                    $"Exact parsing left trailing input. {reader.Remaining} byte(s) remaining."
                    nestedContext
            else
                value
