module Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types

type Issue =
    {
        FieldId : FieldId
        Message : string
    }

type Notice =
    | Info of Issue
    | Warning of Issue

type Failures =
    private Failures of head: Issue * tail: Issue list

type Validation<'a> =
    | Passed of 'a * Notice list
    | Failed of Failures * Notice list

module Failures =

    let single issue =
        Failures (issue, [])

    let append
        (Failures (leftHead, leftTail))
        (Failures (rightHead, rightTail)) =

        Failures (leftHead, leftTail @ (rightHead :: rightTail))

    let toList
        (Failures (head, tail)) =
        head :: tail

let passed value =
    Passed (value, [])

let failed field message =
    Failed (
        Failures.single
            {
                FieldId = field.Id
                Message = message
            },
        []
    )

let info field message =
    Passed (
        (),
        [
            Info
                {
                    FieldId = field.Id
                    Message = message
                }
        ]
    )

let warning field message =
    Passed (
        (),
        [
            Warning
                {
                    FieldId = field.Id
                    Message = message
                }
        ]
    )

let ensure field message condition =
    if condition then
        passed ()
    else
        failed field message

let requireSome message field  =
    match field.Value with
    | Some x -> passed x
    | None -> failed field message

let requireNone message field =
    match field.Value  with
    | Some _ -> failed field message
    | None -> passed ()

let map
    (f: 'a -> 'b)
    (validation: Validation<'a>)
    : Validation<'b> =

    match validation with
    | Passed (value, notices) ->
        Passed (f value, notices)

    | Failed (failures, notices) ->
        Failed (failures, notices)

let bind
    (f: 'a -> Validation<'b>)
    (validation: Validation<'a>)
    : Validation<'b> =

    match validation with
    | Passed (value, notices1) ->
        match f value with
        | Passed (nextValue, notices2) ->
            Passed (nextValue, notices1 @ notices2)

        | Failed (failures, notices2) ->
            Failed (failures, notices1 @ notices2)

    | Failed (failures, notices) ->
        Failed (failures, notices)

let merge
    (left: Validation<'a>)
    (right: Validation<'b>)
    : Validation<'a * 'b> =

    match left, right with
    | Passed (leftValue, leftNotices),
      Passed (rightValue, rightNotices) ->

        Passed (
            (leftValue, rightValue),
            leftNotices @ rightNotices
        )

    | Failed (failures, notices),
      Passed (_, rightNotices) ->

        Failed (
            failures,
            notices @ rightNotices
        )

    | Passed (_, leftNotices),
      Failed (failures, notices) ->

        Failed (
            failures,
            leftNotices @ notices
        )

    | Failed (leftFailures, leftNotices),
      Failed (rightFailures, rightNotices) ->

        Failed (
            Failures.append leftFailures rightFailures,
            leftNotices @ rightNotices
        )

type ValidationBuilder() =

    member _.Return(value: 'a) : Validation<'a> =
        passed value

    member _.ReturnFrom(validation: Validation<'a>) : Validation<'a> =
        validation

    member _.Bind
        (
            validation: Validation<'a>,
            f: 'a -> Validation<'b>
        ) : Validation<'b> =

        bind f validation

    member _.BindReturn
        (
            validation: Validation<'a>,
            f: 'a -> 'b
        ) : Validation<'b> =

        map f validation

    member _.MergeSources
        (
            left: Validation<'a>,
            right: Validation<'b>
        ) : Validation<'a * 'b> =

        merge left right

let validator =
    ValidationBuilder()
