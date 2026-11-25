namespace Metering.Common.Decoding.Parsers

open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Types

type Field<'a> = {
    Id: FieldId
    Span: SourceSpan
    Value: 'a
}

module Field =

    /// <summary>Returns the value carried by the field.</summary>
    let value (field: Field<'a>) = field.Value

    /// <summary>Returns the field's identity.</summary>
    let id (field: Field<'a>) = field.Id

    /// <summary>Returns the field's source identifier, absolute offset and byte length.</summary>
    let span (field: Field<'a>) = field.Span

    /// <summary>Transforms the field's value while preserving its identity and source span.</summary>
    let map f (field: Field<'a>) = {
        Id = field.Id
        Span = field.Span
        Value = f field.Value
    }

    /// <summary>Replaces the field's value while preserving its identity and source span.</summary>
    let withValue value field = map (fun _ -> value) field

    /// <summary>Runs a parser and wraps its result with the supplied field's existing identity and source span.</summary>
    let mapParser parser field =
        parser |>> fun value -> field |> withValue value

module FieldParser =

    /// <summary>Traces a named parser invocation and returns its value with a new field identity and consumed-byte span; records parser failures before propagating them.</summary>
    let parseField (name: string) (inner: Parser<'a>) : Parser<Field<'a>> =

        fun ctx ->
            let start = ctx.Reader.Position

            let startSpan = {
                Source = ctx.Source
                Offset = start
                Length = 0
            }

            let id = ctx.Trace.BeginField(name, startSpan)

            match execute inner ctx with
            | Ok value ->
                let span = {
                    Source = ctx.Source
                    Offset = start
                    Length = ctx.Reader.Position - start
                }

                ctx.Trace.EndField(id, span)

                { Id = id; Span = span; Value = value }
            | Error parserError ->
                let error = parserError |> ParserError.withDefaultSource ctx.Source

                ctx.Trace.FailField(id, error)
                failWith error ctx
