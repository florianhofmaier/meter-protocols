module Metering.Common.Security.Cryptography.Tests.SensitiveBufferTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Security.Cryptography

[<Fact>]
let ``operation sees original buffer contents and return value is preserved`` () =
    let buffer =
        [| 0x01uy; 0x02uy |]

    let result =
        SensitiveBuffer.useZeroed buffer (fun memory ->
            memory.ToArray() |> should equal [| 0x01uy; 0x02uy |]
            42)

    result |> should equal 42

[<Fact>]
let ``buffer is zeroed after successful operation`` () =
    let buffer =
        [| 0x01uy; 0x02uy |]

    SensitiveBuffer.useZeroed buffer (fun _ -> ()) |> ignore

    buffer |> should equal [| 0x00uy; 0x00uy |]

[<Fact>]
let ``buffer is zeroed and original exception is propagated when operation throws`` () =
    let buffer =
        [| 0x01uy; 0x02uy |]

    let ex =
        Assert.Throws<InvalidOperationException>(fun () ->
            SensitiveBuffer.useZeroed buffer (fun _ ->
                raise (InvalidOperationException "programming"))
            |> ignore)

    ex.Message |> should equal "programming"
    buffer |> should equal [| 0x00uy; 0x00uy |]

[<Fact>]
let ``empty buffers are supported`` () =
    let buffer =
        [||]

    let result =
        SensitiveBuffer.useZeroed buffer (fun memory ->
            memory.Length |> should equal 0
            "ok")

    result |> should equal "ok"
    buffer |> should equal [||]
