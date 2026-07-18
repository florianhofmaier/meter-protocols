module Metering.Common.Decoding.Validators.Tests.CoreValidationTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Tests.TestSupport

[<Fact>]
let ``passed contains value and no notices`` () =
    passed 42
    |> should equal (Passed (42, []))

[<Fact>]
let ``failed creates one issue with originating field and message`` () =
    let origin =
        field 3 "raw"

    match failed origin "bad" with
    | Failed (failures, notices) ->
        notices |> should equal ([]: Notice list)
        Failures.toList failures |> should equal [ issue 3 "bad" ]

    | Passed _ ->
        failwith "Expected validation failure"

[<Fact>]
let ``info and warning return passed unit notices`` () =
    info (field 1 "raw") "seen"
    |> should equal (Passed ((), [ Info (issue 1 "seen") ]))

    warning (field 2 "raw") "careful"
    |> should equal (Passed ((), [ Warning (issue 2 "careful") ]))

[<Fact>]
let ``ensure passes and fails according to condition`` () =
    ensure (field 1 "raw") "bad" true |> should equal (Passed ((), []))

    ensure (field 1 "raw") "bad" false
    |> failureMessages
    |> should equal [ "bad" ]

[<Fact>]
let ``requireSome and requireNone validate option shape`` () =
    requireSome "missing" (field 1 (Some 42))
    |> should equal (Passed (42, []))

    requireSome "missing" (field 1 None)
    |> failureMessages
    |> should equal [ "missing" ]

    requireNone "unexpected" (field 2 None)
    |> should equal (Passed ((), []))

    requireNone "unexpected" (field 2 (Some 42))
    |> failureMessages
    |> should equal [ "unexpected" ]

[<Fact>]
let ``failures append preserves left to right order`` () =
    let left =
        Failures.single (issue 1 "left")

    let right =
        Failures.single (issue 2 "right")

    Failures.append left right
    |> Failures.toList
    |> should equal [ issue 1 "left"; issue 2 "right" ]

[<Fact>]
let ``map transforms only passed values and preserves notices`` () =
    map ((+) 1) (Passed (41, [ Info (issue 1 "notice") ]))
    |> should equal (Passed (42, [ Info (issue 1 "notice") ]))

    let mutable invoked =
        false

    let failedValidation =
        Failed (Failures.single (issue 2 "bad"), [ Warning (issue 3 "warn") ])

    map
        (fun value ->
            invoked <- true
            value + 1)
        failedValidation
    |> should equal failedValidation

    invoked |> should equal false

[<Fact>]
let ``bind appends notices and short-circuits failures`` () =
    let result =
        Passed (1, [ Info (issue 1 "first") ])
        |> bind (fun value -> Passed (value + 1, [ Warning (issue 2 "second") ]))

    result
    |> should equal (Passed (2, [ Info (issue 1 "first"); Warning (issue 2 "second") ]))

    let mutable invoked =
        false

    let failedValidation =
        Failed (Failures.single (issue 3 "bad"), [ Info (issue 4 "kept") ])

    failedValidation
    |> bind
        (fun value ->
            invoked <- true
            Passed (value, []))
    |> should equal failedValidation

    invoked |> should equal false

[<Fact>]
let ``passed to failed bind preserves notices from both stages`` () =
    let result =
        Passed (1, [ Info (issue 1 "first") ])
        |> bind (fun _ -> failedWithNotice 2 "bad" (Warning (issue 3 "second")))

    match result with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 2 "bad" ]
        notices |> should equal [ Info (issue 1 "first"); Warning (issue 3 "second") ]

    | Passed _ ->
        failwith "Expected validation failure"

[<Fact>]
let ``merge covers passed and failed combinations`` () =
    merge (Passed (1, [ Info (issue 1 "left") ])) (Passed (2, [ Warning (issue 2 "right") ]))
    |> should equal (Passed ((1, 2), [ Info (issue 1 "left"); Warning (issue 2 "right") ]))

    match merge (Failed (Failures.single (issue 3 "left bad"), [ Info (issue 3 "left notice") ])) (Passed (2, [ Warning (issue 4 "right notice") ])) with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 3 "left bad" ]
        notices |> should equal [ Info (issue 3 "left notice"); Warning (issue 4 "right notice") ]

    | Passed _ ->
        failwith "Expected validation failure"

    match merge (Passed (1, [ Info (issue 5 "left notice") ])) (Failed (Failures.single (issue 6 "right bad"), [ Warning (issue 6 "right notice") ])) with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 6 "right bad" ]
        notices |> should equal [ Info (issue 5 "left notice"); Warning (issue 6 "right notice") ]

    | Passed _ ->
        failwith "Expected validation failure"

[<Fact>]
let ``merge accumulates two failures and notices left to right`` () =
    match merge (failedWithNotice 1 "left bad" (Info (issue 1 "left notice"))) (failedWithNotice 2 "right bad" (Warning (issue 2 "right notice"))) with
    | Failed (failures, notices) ->
        Failures.toList failures |> should equal [ issue 1 "left bad"; issue 2 "right bad" ]
        notices |> should equal [ Info (issue 1 "left notice"); Warning (issue 2 "right notice") ]

    | Passed _ ->
        failwith "Expected validation failure"

[<Fact>]
let ``validator expression supports return returnFrom let and BindReturn`` () =
    validator { return 1 }
    |> should equal (Passed (1, []))

    validator { return! Passed (2, [ Info (issue 1 "notice") ]) }
    |> should equal (Passed (2, [ Info (issue 1 "notice") ]))

    validator {
        let! value = Passed (3, [])
        return value + 1
    }
    |> should equal (Passed (4, []))

    validator {
        let! value = Passed (4, [ Info (issue 2 "notice") ])
        return value + 1
    }
    |> should equal (Passed (5, [ Info (issue 2 "notice") ]))

[<Fact>]
let ``sequential let bang short-circuits`` () =
    let mutable invoked =
        false

    let result =
        validator {
            let! _ = Failed (Failures.single (issue 1 "bad"), [])
            invoked <- true
            return 1
        }

    result |> failureMessages |> should equal [ "bad" ]
    invoked |> should equal false

[<Fact>]
let ``and bang accumulates independent failures`` () =
    let result =
        validator {
            let! left = Failed (Failures.single (issue 1 "left"), [])
            and! right = Failed (Failures.single (issue 2 "right"), [])

            return left + right
        }

    result
    |> failureMessages
    |> should equal [ "left"; "right" ]
