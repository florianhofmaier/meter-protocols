module Metering.Common.Decoding.Validators.Tests.ValidationUtilityTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Common.Decoding.Validators.Tests.TestSupport

[<Fact>]
let ``sequence empty returns passed empty list`` () =
    sequence []
    |> should equal (Passed ([], []))

[<Fact>]
let ``sequence preserves passed value order and notice order`` () =
    sequence [
        Passed (1, [ Info (issue 1 "first") ])
        Passed (2, [ Warning (issue 2 "second") ])
    ]
    |> should equal (Passed ([ 1; 2 ], [ Info (issue 1 "first"); Warning (issue 2 "second") ]))

[<Fact>]
let ``sequence preserves one failure and notices from all inputs`` () =
    match sequence [
        Passed (1, [ Info (issue 1 "first") ])
        Failed (Failures.single (issue 2 "bad"), [ Warning (issue 2 "second") ])
        Passed (3, [ Info (issue 3 "third") ])
    ] with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 2 "bad" ]
        notices |> should equal [ Info (issue 1 "first"); Warning (issue 2 "second"); Info (issue 3 "third") ]

    | Passed (value, _) ->
        failwith $"Expected validation failure, got %A{value}"

[<Fact>]
let ``sequence accumulates multiple failures in input order`` () =
    sequence [
        Failed (Failures.single (issue 1 "first"), [])
        Failed (Failures.single (issue 2 "second"), [])
        Passed (3, [])
    ]
    |> failureMessages
    |> should equal [ "first"; "second" ]

[<Fact>]
let ``traverse invokes validator for all items and preserves output order`` () =
    let seen =
        ResizeArray<int>()

    traverse
        (fun value ->
            seen.Add(value)
            Passed (value * 2, []))
        [ 1; 2; 3 ]
    |> should equal (Passed ([ 2; 4; 6 ], []))

    seen |> Seq.toList |> should equal [ 1; 2; 3 ]

[<Fact>]
let ``traverse accumulates independent failures`` () =
    traverse
        (fun value ->
            if value % 2 = 0 then
                Failed (Failures.single (issue value $"bad {value}"), [])
            else
                Passed (value, []))
        [ 1; 2; 3; 4 ]
    |> failureMessages
    |> should equal [ "bad 2"; "bad 4" ]

[<Fact>]
let ``collectIssues returns unit on success`` () =
    collectIssues [
        Passed ((), [ Info (issue 1 "first") ])
        Passed ((), [ Warning (issue 2 "second") ])
    ]
    |> should equal (Passed ((), [ Info (issue 1 "first"); Warning (issue 2 "second") ]))

[<Fact>]
let ``collectIssues accumulates all failures and notices`` () =
    match collectIssues [
        Passed ((), [ Info (issue 1 "first") ])
        Failed (Failures.single (issue 2 "bad"), [ Warning (issue 2 "second") ])
        Failed (Failures.single (issue 3 "worse"), [ Info (issue 3 "third") ])
    ] with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 2 "bad"; issue 3 "worse" ]
        notices |> should equal [ Info (issue 1 "first"); Warning (issue 2 "second"); Info (issue 3 "third") ]

    | Passed (value, _) ->
        failwith $"Expected validation failure, got %A{value}"
