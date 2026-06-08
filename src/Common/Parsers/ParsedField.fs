namespace Metering.Common.Decoding.Parsers

open Metering.Common.Decoding.Parsers.Types

type ParsedField<'a> =
    {
        Id : FieldId
        Value : 'a
    }

module ParsedField =

    let value (field: ParsedField<'a>) =
        field.Value

    let id (field: ParsedField<'a>) =
        field.Id

    let map f (field: ParsedField<'a>) =
        {
            Id = field.Id
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

            let id =
                ctx.Trace.BeginField(name, start)

            try
                let value =
                    inner ctx

                let span =
                    {
                        Offset = start
                        Length = ctx.Reader.Position - start
                    }

                ctx.Trace.EndField(id, span)

                {
                    Id = id
                    Value = value
                }

            with
            | :? ParserException as ex ->
                ctx.Trace.FailField(id, ex.Error)
                reraise()