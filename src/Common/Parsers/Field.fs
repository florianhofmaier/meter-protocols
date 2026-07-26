namespace Metering.Common.Decoding.Parsers

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

    let withValue field value =
        map (fun _ -> value) field

module FieldParser =

    open Metering.Common.Decoding.Parsers.Core

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
