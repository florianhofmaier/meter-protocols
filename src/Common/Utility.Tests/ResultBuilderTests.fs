module Metering.Common.Utility.Tests.ResultBuilderTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Utility.Result

[<Fact>]
let ``return creates ok`` () =
    result { return 42 }
    |> should equal (Ok 42)

[<Fact>]
let ``return from preserves existing result`` () =
    result { return! Ok 42 }
    |> should equal (Ok 42)

    result { return! Error "bad" }
    |> should equal (Error "bad")

[<Fact>]
let ``successful let bang passes value onward`` () =
    result {
        let! value = Ok 41
        return value + 1
    }
    |> should equal (Ok 42)

[<Fact>]
let ``failed let bang short-circuits later code`` () =
    let mutable invoked =
        false

    let output =
        result {
            let! _ = Error "bad"
            invoked <- true
            return 42
        }

    output |> should equal (Error "bad": Result<int, string>)
    invoked |> should equal false

[<Fact>]
let ``zero returns ok unit`` () =
    result.Zero()
    |> should equal (Ok ())
