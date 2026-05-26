namespace Metering.Common.Validators

open Metering.Common.Parsers.ParserTree

type Issue =
    {
        Node : ParsedNode
        Message : string
    }

[<RequireQualifiedAccess>]
type Notice =
    | Info of Issue
    | Warning of Issue

type Failures =
    private
        Failures of head: Issue * tail: Issue list

module Failures =

    let create issue =
        Failures (issue, [])

    let toList (Failures (head, tail)) =
        head :: tail

    let append
        (Failures (leftHead, leftTail))
        (Failures (rightHead, rightTail)) =

        Failures (leftHead, leftTail @ (rightHead :: rightTail))

type ValidationReport<'a> =
    | Passed of value: 'a * notices: Notice list
    | Failed of failures: Failures * notices: Notice list

module ValidationReport =

    let map
        (f: 'a -> 'b)
        (report: ValidationReport<'a>)
        : ValidationReport<'b> =

        match report with
        | Passed (value, notices) ->
            Passed (f value, notices)

        | Failed (failures, notices) ->
            Failed (failures, notices)