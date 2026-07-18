module Metering.Common.Decoding.Parsers.Tests.TypesTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.Types

[<Fact>]
let ``source id helpers preserve known and unknown identity`` () =
    SourceId.root |> should equal (SourceId.create 0)
    SourceId.unknown |> should equal (SourceId.create -1)
    SourceId.isUnknown SourceId.unknown |> should equal true
    SourceId.isUnknown SourceId.root |> should equal false
    SourceId.create 42 |> should equal { Value = 42 }

[<Fact>]
let ``parser error default source only replaces unknown source`` () =
    let fallback =
        SourceId.create 12

    let unknownError =
        {
            Source = SourceId.unknown
            Pos = 7
            Msg = "bad"
        }

    let explicitError =
        {
            Source = SourceId.create 99
            Pos = 8
            Msg = "worse"
        }

    ParserError.withDefaultSource fallback unknownError
    |> should equal { unknownError with Source = fallback }

    ParserError.withDefaultSource fallback explicitError
    |> should equal explicitError

[<Fact>]
let ``parser error default source keeps position and message unchanged`` () =
    let error =
        {
            Source = SourceId.unknown
            Pos = 123
            Msg = "same"
        }

    let mapped =
        ParserError.withDefaultSource (SourceId.create 4) error

    mapped.Pos |> should equal 123
    mapped.Msg |> should equal "same"

[<Fact>]
let ``field ids compare by their underlying value`` () =
    let left =
        FieldId.create 1

    let same =
        FieldId.create 1

    let right =
        FieldId.create 2

    left |> should equal same
    compare left right |> should be (lessThan 0)
    compare right left |> should be (greaterThan 0)

[<Fact>]
let ``source span and transform preserve supplied data`` () =
    let span =
        {
            Source = SourceId.create 7
            Offset = 11
            Length = 13
        }

    span.Source |> should equal (SourceId.create 7)
    span.Offset |> should equal 11
    span.Length |> should equal 13
    SourceTransform.Root |> should equal SourceTransform.Root
    SourceTransform.Decrypt "AES" |> should equal (SourceTransform.Decrypt "AES")
    SourceTransform.Decompress "LZ" |> should equal (SourceTransform.Decompress "LZ")
