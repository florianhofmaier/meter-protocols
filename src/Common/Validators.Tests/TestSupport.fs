module Metering.Common.Decoding.Validators.Tests.TestSupport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core

let field id value =
    {
        Id = FieldId.create id
        Span =
            {
                Source = SourceId.create 10
                Offset = id
                Length = 1
            }
        Value = value
    }

let issue id message =
    {
        FieldId = FieldId.create id
        Message = message
    }

let passedWithNotice value notice =
    Passed (value, [ notice ])

let failedWithNotice id failureMessage notice =
    Failed (Failures.single (issue id failureMessage), [ notice ])

let failureMessages =
    function
    | Failed (failures, _) ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)

    | Passed (value, _) ->
        failwith $"Expected failure, got %A{value}"

let notices =
    function
    | Passed (_, notices)
    | Failed (_, notices) ->
        notices
