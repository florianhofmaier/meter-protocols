namespace Metering.Mbus.Analysis

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

/// Input boundaries and the recorded operations that made them available.
type internal AnalysisInput = {
    Span: SourceSpan
    Dependencies: AnalysisStepId list
}

/// A recorded operation always has an ID; only success provides a value.
[<RequireQualifiedAccess>]
type internal AnalysisOperation<'value> =
    | Succeeded of value: 'value * stepId: AnalysisStepId
    | Failed of stepId: AnalysisStepId

    member this.StepId: AnalysisStepId =
        match this with
        | AnalysisOperation.Succeeded(_, stepId)
        | AnalysisOperation.Failed stepId -> stepId

    member this.Value: 'value option =
        match this with
        | AnalysisOperation.Succeeded(value, _) -> Some value
        | AnalysisOperation.Failed _ -> None

/// Absence permits continuation without recording an artificial parse step.
[<RequireQualifiedAccess>]
type internal OptionalParse<'value> =
    | Absent of following: AnalysisInput
    | Present of value: 'value * following: AnalysisInput
    | Failed of stepId: AnalysisStepId

/// Owns the sources, trace and recorded steps of one analysis; no protocol routing.
type internal AnalysisSession(bytes: ReadOnlyMemory<byte>) =
    let collector = ParseTraceCollector()
    let trace = collector :> IFieldTracer

    let sources =
        InMemorySourceStore(fun bytes offset -> ByteReaderFactory.Create(bytes, offset))
        :> ISourceStore

    let root = sources.AddRoot "M-Bus frame" bytes false

    let span = {
        Source = root.Id
        Offset = 0
        Length = bytes.Length
    }

    let input =
        trace.SourceCreated root
        let id = trace.BeginField("Input bytes", { span with Length = 0 })
        trace.EndField(id, span)
        { Id = id; Span = span; Value = bytes }

    let context = {
        CreateReader = fun bytes offset -> ByteReaderFactory.Create(bytes, offset)
        Sources = sources
        Trace = trace
    }

    let steps = ResizeArray<MbusAnalysisStep>()

    let isFailure (step: MbusAnalysisStep) =
        match step.Status with
        | AnalysisStepStatus.Succeeded -> false
        | _ -> true

    member _.Input: Field<ReadOnlyMemory<byte>> = input
    member _.HasFailures: bool = steps |> Seq.exists isFailure
    member _.FirstFailureId: AnalysisStepId = (steps |> Seq.find isFailure).Metadata.Id

    member private _.Add
        (recordStep: AnalysisStep<'value> -> MbusAnalysisStep)
        dependencies
        input
        result
        notices
        =
        let id = AnalysisStepId.Create steps.Count

        let metadata = {
            Id = id
            Input = input
            Dependencies = dependencies
            Notices = notices
        }

        steps.Add(recordStep { Metadata = metadata; Result = result })
        id

    member private this.Run
        (recordStep: AnalysisStep<'stored> -> MbusAnalysisStep)
        (dependencies: AnalysisStepId list)
        (input: SourceSpan)
        (decode: Decoder<'value>)
        (recordValue: 'value -> 'stored)
        : AnalysisOperation<'value> =
        match decode context with
        | Decoded(value, notices) ->
            let stepId =
                this.Add
                    recordStep
                    dependencies
                    input
                    (AnalysisStepResult.Succeeded(recordValue value))
                    notices

            AnalysisOperation.Succeeded(value, stepId)
        | DecodeFailed(error, notices) ->
            AnalysisOperation.Failed(
                this.Add recordStep dependencies input (AnalysisStepResult.Failed error) notices
            )

    member this.Execute
        (recordStep: AnalysisStep<'value> -> MbusAnalysisStep)
        (dependencies: AnalysisStepId list)
        (input: SourceSpan)
        (decode: Decoder<'value>)
        : AnalysisOperation<'value> =
        this.Run recordStep dependencies input decode id

    member private _.ParseRange
        (source: SourceSpan)
        (partParser: Parser<'value>)
        : Decoder<'value> =
        fun _ ->
            let reader = sources.CreateReader source.Source

            let parsed =
                reader.Skip(source.Offset - reader.Position)
                |> Result.bind (fun () -> reader.Slice source.Length)
                |> Result.mapError (fun error -> {
                    Source = source.Source
                    Pos = error.Pos
                    Msg = error.Msg
                })
                |> Result.bind (fun bounded ->
                    ParserRunner.runExactlyWithSource
                        source.Source
                        bounded
                        sources
                        trace
                        partParser)

            match parsed with
            | Ok value -> Decoded(value, [])
            | Error error -> DecodeFailed(ParseFailed error, [])

    member this.Parse
        (recordStep: AnalysisStep<unit> -> MbusAnalysisStep)
        (input: AnalysisInput)
        (parser: Parser<'value>)
        : AnalysisOperation<'value> =
        this.Run recordStep input.Dependencies input.Span (this.ParseRange input.Span parser) ignore

    member this.Validate
        (recordStep: AnalysisStep<'value> -> MbusAnalysisStep)
        (dependencies: AnalysisStepId list)
        (raw: Field<'raw>)
        (validator: Field<'raw> -> Validation<'value>)
        : AnalysisOperation<'value> =
        this.Execute recordStep dependencies raw.Span (Decoder.validate validator raw)

    // Unread bytes are a source range, not an additional protocol field.
    // Absence creates no step or new field.
    member this.ParseOptional
        (recordStep: AnalysisStep<unit> -> MbusAnalysisStep)
        (input: AnalysisInput)
        (partParser: Parser<'value option>)
        : OptionalParse<'value> =
        let source = input.Span

        let parserWithRemainder =
            parser {
                let! raw = partParser
                let! offset = position
                let! length = remaining
                do! skip length

                return
                    raw,
                    {
                        Source = source.Source
                        Offset = offset
                        Length = length
                    }
            }

        match this.ParseRange source parserWithRemainder context with
        | Decoded((None, _), _) -> OptionalParse.Absent input
        | Decoded((Some raw, following), notices) ->
            let id =
                this.Add
                    recordStep
                    input.Dependencies
                    source
                    (AnalysisStepResult.Succeeded())
                    notices

            OptionalParse.Present(
                raw,
                {
                    Span = following
                    Dependencies = [ id ]
                }
            )
        | DecodeFailed(error, notices) ->
            OptionalParse.Failed(
                this.Add
                    recordStep
                    input.Dependencies
                    source
                    (AnalysisStepResult.Failed error)
                    notices
            )

    member this.Block
        (recordStep: AnalysisStep<'value> -> MbusAnalysisStep)
        (input: SourceSpan)
        (cause: AnalysisStepId)
        (reason: string)
        : unit =
        this.Add recordStep [ cause ] input (AnalysisStepResult.Blocked(cause, reason)) []
        |> ignore

    member _.Snapshot() : MbusAnalysis =
        let trace = collector.Snapshot()

        let sourceData =
            trace.Sources
            |> List.map (fun info -> {
                Info = info
                Bytes = (sources.CreateReader info.Id).Buffer
            })

        {
            Trace = trace
            Steps = steps |> Seq.toList
            Sources = sourceData
        }
