namespace Metering.Common.Decoding.Parsers

open Metering.Common.Decoding.Parsers.Types

type ParsedField<'a> =
    {
        Id : FieldId
        Span : SourceSpan
        Value : 'a
    }

module ParsedField =

    let value (field: ParsedField<'a>) =
        field.Value

    let id (field: ParsedField<'a>) =
        field.Id

    let span (field: ParsedField<'a>) =
        field.Span

    let map f (field: ParsedField<'a>) =
        {
            Id = field.Id
            Span = field.Span
            Value = f field.Value
        }

module FieldParser =

    open Metering.Common.Decoding.Parsers.Core

    let parseField
        (name: string)
        (inner: Parser<'a>)
        : Parser<ParsedField<'a>> =

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

            try
                let value =
                    inner ctx

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

            with
            | :? ParserException as ex ->
                let error =
                    ex.Error
                    |> ParserError.withDefaultSource ctx.Source

                ctx.Trace.FailField(id, error)
                raise (ParserException error)
