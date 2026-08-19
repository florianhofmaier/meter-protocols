namespace Metering.Common.Decoding.Parsers

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Types

type Field<'a> =
    {
        Id : FieldId
        Span : SourceSpan
        Value : 'a
    }

module Field =

    let value (field: Field<'a>) =
        field.Value

    let id (field: Field<'a>) =
        field.Id

    let span (field: Field<'a>) =
        field.Span

    let map f (field: Field<'a>) =
        {
            Id = field.Id
            Span = field.Span
            Value = f field.Value
        }

    let withValue value field =
        map (fun _ -> value) field

    let mapParser parser field =
        parser
        |>> fun value ->
            field |> withValue value

module FieldParser =

    let parseField
        (name: string)
        (inner: Parser<'a>)
        : Parser<Field<'a>> =

        fun ctx ->
            let start =
                ctx.Reader.Position

            let startSpan =
                {
                    Source = ctx.Source
                    Offset = start
                    Length = 0
                }

            let id =
                ctx.Trace.BeginField(name, startSpan)

            match execute inner ctx with
            | Ok value ->
                let span =
                    {
                        Source = ctx.Source
                        Offset = start
                        Length = ctx.Reader.Position - start
                    }

                ctx.Trace.EndField(id, span)

                {
                    Id = id
                    Span = span
                    Value = value
                }
            | Error parserError ->
                let error =
                    parserError
                    |> ParserError.withDefaultSource ctx.Source

                ctx.Trace.FailField(id, error)
                failWith error ctx
