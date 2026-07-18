module Metering.Common.Decoding.Decoders.Tests.CoreDecoderTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Tests.TestSupport
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

module DecoderCore =
    Metering.Common.Decoding.Decoders.Core.Core

let private baseContext () =
    let readerFactory =
        RecordingReaderFactory()

    let tracer =
        RecordingTracer()

    context None readerFactory (tracer :> IFieldTracer), readerFactory, tracer

[<Fact>]
let ``decodePassed returns value without notices`` () =
    let ctx, _, _ =
        baseContext ()

    DecoderCore.decodePassed 42 ctx
    |> should equal (Decoded (42, []))

[<Fact>]
let ``decodeError returns exact failure without notices`` () =
    let failure =
        EncryptionFailed (issue 1 "bad")

    let ctx, _, _ =
        baseContext ()

    DecoderCore.decodeError failure ctx
    |> should equal (DecodeFailed (failure, []))

[<Fact>]
let ``parse returns parsed field on exact consumption`` () =
    let ctx, readerFactory, tracer =
        baseContext ()

    let source =
        sourceField 3 7 25 [| 0xAAuy |]

    match DecoderCore.parse (parseField "byte" parseU8) source ctx with
    | Decoded (field, notices) ->
        notices |> should equal ([]: Notice list)
        field.Id |> should equal (FieldId.create 0)
        field.Span |> should equal { Source = SourceId.create 7; Offset = 25; Length = 1 }
        field.Value |> should equal 0xAAuy
        readerFactory.Calls |> should equal [ { Bytes = [| 0xAAuy |]; Offset = 25 } ]

        tracer.Events
        |> should equal [
            BeginField ("byte", { Source = SourceId.create 7; Offset = 25; Length = 0 }, FieldId.create 0)
            EndField (FieldId.create 0, field.Span)
        ]

    | actual ->
        failwith $"Expected decoded field, got %A{actual}"

[<Fact>]
let ``parseValue returns parsed value on exact consumption`` () =
    let ctx, readerFactory, _ =
        baseContext ()

    let source =
        sourceField 3 7 25 [| 0xAAuy |]

    match DecoderCore.parseValue parseU8 source ctx with
    | Decoded (value, notices) ->
        value |> should equal 0xAAuy
        notices |> should equal ([]: Notice list)
        readerFactory.Calls |> should equal [ { Bytes = [| 0xAAuy |]; Offset = 25 } ]

    | actual ->
        failwith $"Expected decoded value, got %A{actual}"

[<Fact>]
let ``parse and parseValue reject trailing bytes`` () =
    let ctx, _, _ =
        baseContext ()

    let source =
        sourceField 3 7 25 [| 0xAAuy; 0xBBuy |]

    match DecoderCore.parseValue parseU8 source ctx with
    | DecodeFailed (ParseFailed error, []) ->
        error.Source |> should equal (SourceId.create 7)
        error.Pos |> should equal 26
        error.Msg |> should equal "Exact parsing left trailing input. 1 byte(s) remaining."

    | actual ->
        failwith $"Expected parse failure, got %A{actual}"

    match DecoderCore.parse (parseField "byte" parseU8) source ctx with
    | DecodeFailed (ParseFailed error, _) ->
        error.Pos |> should equal 26

    | actual ->
        failwith $"Expected parse failure, got %A{actual}"

[<Fact>]
let ``parser failure maps to parse failed with source and offset preserved`` () =
    let ctx, _, _ =
        baseContext ()

    let source =
        sourceField 3 7 25 [| 0xAAuy |]

    match DecoderCore.parseValue (take 2) source ctx with
    | DecodeFailed (ParseFailed error, []) ->
        error.Source |> should equal (SourceId.create 7)
        error.Pos |> should equal 25
        error.Msg.Contains("Unexpected end of buffer") |> should equal true

    | actual ->
        failwith $"Expected parser failure, got %A{actual}"

[<Fact>]
let ``validate maps passed and failed validations while preserving notices`` () =
    let ctx, _, _ =
        baseContext ()

    let raw =
        sourceField 3 7 25 [| 0xAAuy |]

    match DecoderCore.validate (fun (field: Field<ReadOnlyMemory<byte>>) -> Passed (field.Value.Length, [ Info (issue 1 "notice") ])) raw ctx with
    | Decoded (value, notices) ->
        value |> should equal 1
        notices |> should equal [ Info (issue 1 "notice") ]

    | actual ->
        failwith $"Expected decoded value, got %A{actual}"

    match DecoderCore.validate (fun _ -> Failed (Failures.single (issue 2 "bad"), [ Warning (issue 3 "warn") ])) raw ctx with
    | DecodeFailed (ValidationFailed failures, notices) ->
        Failures.toList failures |> should equal [ issue 2 "bad" ]
        notices |> should equal [ Warning (issue 3 "warn") ]

    | actual ->
        failwith $"Expected validation failure, got %A{actual}"

[<Fact>]
let ``createDerivedSource without source store returns structured failure attached to origin`` () =
    let ctx, _, _ =
        baseContext ()

    let origin =
        sourceField 12 7 25 [| 0xAAuy |]

    match DecoderCore.createDerivedSource "derived" SourceTransform.Root false origin (memory [| 0x01uy |]) ctx with
    | DecodeFailed (EncryptionFailed issue, []) ->
        issue.FieldId |> should equal origin.Id
        issue.Message |> should equal "decode source store is required to register a derived source"

    | actual ->
        failwith $"Expected structured failure, got %A{actual}"

[<Fact>]
let ``createDerivedSource registers source traces creation and returns derived field`` () =
    let readerFactory =
        RecordingReaderFactory()

    let tracer =
        RecordingTracer()

    let store =
        CapturingSourceStore(SourceId.create 100)

    let ctx =
        context (Some (store :> ISourceStore)) readerFactory (tracer :> IFieldTracer)

    let origin =
        sourceField 12 7 25 [| 0xAAuy; 0xBBuy |]

    let bytes =
        memory [| 0x01uy; 0x02uy |]

    match DecoderCore.createDerivedSource "derived" (SourceTransform.Decrypt "AES") true origin bytes ctx with
    | Decoded (derived, notices) ->
        notices |> should equal ([]: Notice list)
        derived.Id |> should equal origin.Id
        derived.Span |> should equal { Source = SourceId.create 100; Offset = 0; Length = 2 }
        derived.Value.ToArray() |> should equal [| 0x01uy; 0x02uy |]

        store.AddDerivedArgs
        |> should equal (Some ("derived", origin.Span, SourceTransform.Decrypt "AES", [| 0x01uy; 0x02uy |], true))

        tracer.Events
        |> should equal [
            SourceCreated {
                Id = SourceId.create 100
                Name = "derived"
                Origin = Some origin.Span
                Transform = SourceTransform.Decrypt "AES"
                Length = 2
                Sensitive = true
            }
        ]

    | actual ->
        failwith $"Expected derived source, got %A{actual}"

[<Fact>]
let ``createDerivedSource supports empty derived bytes`` () =
    let readerFactory =
        RecordingReaderFactory()

    let tracer =
        RecordingTracer()

    let store =
        CapturingSourceStore(SourceId.create 101)

    let ctx =
        context (Some (store :> ISourceStore)) readerFactory (tracer :> IFieldTracer)

    let origin =
        sourceField 12 7 25 [| 0xAAuy |]

    match DecoderCore.createDerivedSource "empty" SourceTransform.Root false origin (memory [||]) ctx with
    | Decoded (derived, _) ->
        derived.Span.Length |> should equal 0
        derived.Value.ToArray() |> should equal [||]

    | actual ->
        failwith $"Expected derived source, got %A{actual}"

[<Fact>]
let ``decoder expression supports return returnFrom and successful bind`` () =
    let ctx, _, _ =
        baseContext ()

    (decoder { return 1 }) ctx
    |> should equal (Decoded (1, []))

    (decoder { return! fun _ -> Decoded (2, [ Info (issue 1 "notice") ]) }) ctx
    |> should equal (Decoded (2, [ Info (issue 1 "notice") ]))

    (decoder {
        let! value = fun _ -> Decoded (3, [ Info (issue 2 "first") ])
        return value + 1
    }) ctx
    |> should equal (Decoded (4, [ Info (issue 2 "first") ]))

[<Fact>]
let ``successful bind and later failure preserve notice order`` () =
    let ctx, _, _ =
        baseContext ()

    let success =
        decoder {
            let! first = fun _ -> Decoded (1, [ Info (issue 1 "first") ])
            let! second = fun _ -> Decoded (2, [ Warning (issue 2 "second") ])
            return first + second
        }

    success ctx
    |> should equal (Decoded (3, [ Info (issue 1 "first"); Warning (issue 2 "second") ]))

    let failure =
        decoder {
            let! _ = fun _ -> Decoded (1, [ Info (issue 1 "first") ])
            return! fun _ -> DecodeFailed (EncryptionFailed (issue 2 "bad"), [ Warning (issue 2 "second") ])
        }

    failure ctx
    |> should equal (DecodeFailed (EncryptionFailed (issue 2 "bad"), [ Info (issue 1 "first"); Warning (issue 2 "second") ]))

[<Fact>]
let ``initial decoder failure short-circuits without invoking next decoder`` () =
    let ctx, _, _ =
        baseContext ()

    let mutable invoked =
        false

    let result =
        (decoder {
            let! _ = fun _ -> DecodeFailed (EncryptionFailed (issue 1 "bad"), [ Info (issue 1 "first") ])
            invoked <- true
            return 1
        }) ctx

    match result with
    | DecodeFailed (EncryptionFailed failureIssue, notices) ->
        failureIssue |> should equal (issue 1 "bad")
        notices |> should equal [ Info (issue 1 "first") ]

    | actual ->
        failwith $"Expected initial decoder failure, got %A{actual}"

    invoked |> should equal false

[<Fact>]
let ``decoder programming exceptions are not swallowed`` () =
    let ctx, _, _ =
        baseContext ()

    let throwingDecoder : Decoder<int> =
        fun _ ->
            raise (InvalidOperationException "programming")

    let ex =
        Assert.Throws<InvalidOperationException>(fun () ->
            throwingDecoder ctx |> ignore)

    ex.Message |> should equal "programming"
