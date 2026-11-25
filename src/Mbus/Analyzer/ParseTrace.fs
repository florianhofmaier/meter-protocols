namespace Metering.Mbus.Analysis

open System.Collections.Generic
open Metering.Common.Decoding.Parsers.Types

/// A failed field has a known start, but no successfully delimited full span.
[<RequireQualifiedAccess>]
type ParseFieldResult =
    | Reading of start: SourceSpan
    | Parsed of span: SourceSpan
    | Failed of start: SourceSpan * error: ParserError

/// Structural observation only. Raw bytes remain in the referenced source.
type ParseField = {
    Id: FieldId
    Name: string
    Result: ParseFieldResult
    Children: ParseField list
}

/// Structural snapshot; source bytes are retained separately by the report or source owner.
type ParseTrace = {
    Sources: SourceInfo list
    Fields: ParseField list
}

type private OpenField = {
    Id: FieldId
    Name: string
    Start: SourceSpan
    mutable Result: ParseFieldResult
    Children: ResizeArray<OpenField>
}

/// Collects a single sequential parse/decode run through the existing tracer API.
/// Register the root source with SourceCreated before starting the run.
type ParseTraceCollector() =

    let sources = ResizeArray<SourceInfo>()
    let fields = ResizeArray<OpenField>()
    let active = Stack<OpenField>()
    let mutable nextId = 0

    let finish id result =
        if active.Count = 0 || active.Peek().Id <> id then
            invalidOp "Trace completion must match the innermost open field."

        let field = active.Pop()
        field.Result <- result field.Start

    /// Later tracer events cannot change an earlier snapshot.
    member _.Snapshot() : ParseTrace =
        let rec snapshot (field: OpenField) : ParseField = {
            Id = field.Id
            Name = field.Name
            Result = field.Result
            Children = field.Children |> Seq.map snapshot |> Seq.toList
        }

        {
            Sources = sources |> Seq.toList
            Fields = fields |> Seq.map snapshot |> Seq.toList
        }

    interface IFieldTracer with
        member _.SourceCreated source = sources.Add source

        member _.BeginField(name, start) =
            let id = FieldId.create nextId
            nextId <- nextId + 1

            let field = {
                Id = id
                Name = name
                Start = start
                Result = ParseFieldResult.Reading start
                Children = ResizeArray()
            }

            if active.Count = 0 then
                fields.Add field
            else
                active.Peek().Children.Add field

            active.Push field
            id

        member _.EndField(id, span) =
            finish id (fun _ -> ParseFieldResult.Parsed span)

        member _.FailField(id, error) =
            finish id (fun start -> ParseFieldResult.Failed(start, error))
