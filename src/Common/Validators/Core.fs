module Metering.Common.Decoding.Validators.Core

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types

type Issue = { FieldId: FieldId; Message: string }

type Notice =
    | Info of Issue
    | Warning of Issue

type Failures = private Failures of head: Issue * tail: Issue list

type Validation<'a> =
    | Passed of 'a * Notice list
    | Failed of Failures * Notice list

module Failures =

    /// <summary>Creates a non-empty failure collection containing one issue.</summary>
    let single issue = Failures(issue, [])

    /// <summary>Combines two failure collections, preserving their left-to-right issue order.</summary>
    let append (Failures(leftHead, leftTail)) (Failures(rightHead, rightTail)) =

        Failures(leftHead, leftTail @ (rightHead :: rightTail))

    /// <summary>Returns all issues in their stored order as a non-empty list.</summary>
    let toList (Failures(head, tail)) = head :: tail

/// <summary>Creates a successful validation result without notices.</summary>
let passed value = Passed(value, [])

/// <summary>Creates a failure collection with one issue associated with the supplied field's identity.</summary>
let failure (field: Field<_>) message =
    Failures.single (
        {
            FieldId = field.Id
            Message = message
        }
        : Issue
    )

/// <summary>Creates a failed validation result for the supplied field without notices.</summary>
let failed (field: Field<_>) message = Failed(failure field message, [])

/// <summary>Returns a successful unit result with one informational notice for the supplied field.</summary>
let info (field: Field<_>) message =
    Passed(
        (),
        [
            Info(
                {
                    FieldId = field.Id
                    Message = message
                }
                : Issue
            )
        ]
    )

/// <summary>Returns a successful unit result with one warning notice for the supplied field.</summary>
let warning (field: Field<_>) message =
    Passed(
        (),
        [
            Warning(
                {
                    FieldId = field.Id
                    Message = message
                }
                : Issue
            )
        ]
    )

/// <summary>Returns unit when the condition holds; otherwise fails with an issue for the supplied field.</summary>
let ensure (field: Field<_>) message condition =
    if condition then passed () else failed field message

/// <summary>Returns the field's optional value when present; otherwise fails with an issue for that field.</summary>
let requireSome message (field: Field<_>) =
    match field.Value with
    | Some x -> passed x
    | None -> failed field message

/// <summary>Returns unit when the field's optional value is absent; otherwise fails with an issue for that field.</summary>
let requireNone message (field: Field<_>) =
    match field.Value with
    | Some _ -> failed field message
    | None -> passed ()

/// <summary>Transforms a successful value while preserving failures and notices unchanged.</summary>
let map (f: 'a -> 'b) (validation: Validation<'a>) : Validation<'b> =

    match validation with
    | Passed(value, notices) -> Passed(f value, notices)

    | Failed(failures, notices) -> Failed(failures, notices)

/// <summary>Runs the next validation only after success, preserving notices in execution order.</summary>
let bind (f: 'a -> Validation<'b>) (validation: Validation<'a>) : Validation<'b> =

    match validation with
    | Passed(value, notices1) ->
        match f value with
        | Passed(nextValue, notices2) -> Passed(nextValue, notices1 @ notices2)

        | Failed(failures, notices2) -> Failed(failures, notices1 @ notices2)

    | Failed(failures, notices) -> Failed(failures, notices)

/// <summary>Combines independent validation results, accumulating both failures and notices in left-to-right order.</summary>
let merge (left: Validation<'a>) (right: Validation<'b>) : Validation<'a * 'b> =

    match left, right with
    | Passed(leftValue, leftNotices), Passed(rightValue, rightNotices) ->

        Passed((leftValue, rightValue), leftNotices @ rightNotices)

    | Failed(failures, notices), Passed(_, rightNotices) ->

        Failed(failures, notices @ rightNotices)

    | Passed(_, leftNotices), Failed(failures, notices) ->

        Failed(failures, leftNotices @ notices)

    | Failed(leftFailures, leftNotices), Failed(rightFailures, rightNotices) ->

        Failed(Failures.append leftFailures rightFailures, leftNotices @ rightNotices)

type ValidationBuilder() =

    /// <summary>Returns a successful validation result without notices.</summary>
    member _.Return(value: 'a) : Validation<'a> = passed value

    /// <summary>Uses an existing validation result as the result of the computation expression.</summary>
    member _.ReturnFrom(validation: Validation<'a>) : Validation<'a> = validation

    /// <summary>Continues validation after success, preserving notices and stopping on failure.</summary>
    member _.Bind(validation: Validation<'a>, f: 'a -> Validation<'b>) : Validation<'b> =

        bind f validation

    /// <summary>Transforms a successful value for a final return expression, preserving failures and notices.</summary>
    member _.BindReturn(validation: Validation<'a>, f: 'a -> 'b) : Validation<'b> =

        map f validation

    /// <summary>Combines and! sources, accumulating independent failures and notices in left-to-right order.</summary>
    member _.MergeSources(left: Validation<'a>, right: Validation<'b>) : Validation<'a * 'b> =

        merge left right

/// <summary>Builds validation computation expressions with sequential binds and applicative and! sources.</summary>
let validator = ValidationBuilder()
