module Metering.Common.Decoding.Parsers.Tests.CoreParserTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Tests.TestSupport

let private runOk parser bytes =
    match runExactly (reader bytes) trace parser with
    | Ok value ->
        value

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``map transforms value and preserves parser consumption`` () =
    let r = reader [| 0x02uy |]

    match runExactly r trace (map parseU8 ((+) 1uy)) with
    | Ok value ->
        value |> should equal 0x03uy
        r.Position |> should equal 1
        r.Remaining |> should equal 0

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``bind passes first value to second parser and sequences consumption`` () =
    let parser =
        bind parseU8 (fun first ->
            parseU8
            |>> fun second -> first, second)

    runOk parser [| 0xAAuy; 0xBBuy |]
    |> should equal (0xAAuy, 0xBBuy)

[<Fact>]
let ``operators behave like map and bind`` () =
    let mapped =
        parseU8 |>> int

    let bound =
        parseU8 >>= fun first -> parseU8 |>> fun second -> first + second

    runOk mapped [| 0x05uy |] |> should equal 5
    runOk bound [| 0x05uy; 0x06uy |] |> should equal 0x0Buy

[<Fact>]
let ``parser return consumes no input`` () =
    let r = reader [| 0xAAuy |]

    match run r trace (parser { return 42 }) with
    | Ok value ->
        value |> should equal 42
        r.Position |> should equal 0
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``return from executes returned parser`` () =
    runOk (parser { return! parseU8 }) [| 0xAAuy |]
    |> should equal 0xAAuy

[<Fact>]
let ``let bang sequences parsers correctly`` () =
    let parser =
        parser {
            let! first = parseU8
            let! second = parseU8
            return first, second
        }

    runOk parser [| 0x01uy; 0x02uy |]
    |> should equal (0x01uy, 0x02uy)

[<Fact>]
let ``BindReturn maps without changing parser semantics`` () =
    let parser =
        parser {
            let! value = parseU8
            return value + 1uy
        }

    runOk parser [| 0x01uy |]
    |> should equal 0x02uy

[<Fact>]
let ``Zero consumes no input`` () =
    let r = reader [| 0xAAuy |]

    match run r trace (parser.Zero()) with
    | Ok () ->
        r.Position |> should equal 0
        r.Remaining |> should equal 1

    | Error error ->
        failwith $"Unexpected parser error: {error.Msg}"

[<Fact>]
let ``Combine executes first parser before second`` () =
    let observed =
        ResizeArray<string>()

    let first : Parser<unit> =
        fun _ ->
            observed.Add("first")

    let second : Parser<int> =
        fun _ ->
            observed.Add("second")
            7

    runOk (parser.Combine(first, second)) [||]
    |> should equal 7

    observed |> Seq.toList |> should equal [ "first"; "second" ]

[<Fact>]
let ``parser failure prevents later parser execution`` () =
    let mutable executed =
        false

    let later : Parser<int> =
        fun _ ->
            executed <- true
            1

    let failing =
        parser {
            do! fail "stop"
            return! later
        }

    match runExactly (reader [||]) trace failing with
    | Error error ->
        error.Msg |> should equal "stop"
        executed |> should equal false

    | Ok value ->
        failwith $"Expected parser failure, got {value}"

[<Fact>]
let ``delayed parser body is not evaluated before parser execution`` () =
    let mutable evaluated =
        false

    let delayed =
        parser {
            evaluated <- true
            return 42
        }

    evaluated |> should equal false

    runOk delayed [||]
    |> should equal 42

    evaluated |> should equal true
