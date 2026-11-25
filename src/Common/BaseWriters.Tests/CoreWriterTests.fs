module Metering.Common.BaseWriters.Tests.CoreWriterTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

let private bytes =
    function
    | Ok value -> value
    | Error error -> failwith $"Unexpected writer error: {error}"

[<Fact>]
let ``runner returns only written bytes from an independent buffer`` () =
    let mutable retained = [||]

    let write =
        writer {
            do!
                fun state ->
                    retained <- state.Buf
                    Ok((), state)

            do! writeU16LittleEndian 0x1234us
        }

    let result = Writer.run 20 write |> bytes
    result |> should equal [| 0x34uy; 0x12uy |]
    retained[0] <- 0uy
    result[0] |> should equal 0x34uy
    result[1] <- 0uy
    retained[1] |> should equal 0x12uy

[<Fact>]
let ``runner supports empty output and explicitly sized buffers beyond the default`` () =
    Writer.run 0 (Writer.ret ()) |> bytes |> Array.isEmpty |> should be True
    let input = Array.init 513 (fun index -> byte (index % 256))

    Writer.run input.Length (writeMemory (ReadOnlyMemory input))
    |> bytes
    |> should equal input

[<Fact>]
let ``runner preserves errors without retrying or running later writes`` () =
    let mutable calls = 0

    let expected = {
        Pos = 1
        Msg = "stopped"
        Ctx = [ "test" ]
    }

    let write =
        writer {
            do!
                fun state ->
                    calls <- calls + 1
                    Ok((), state)

            do! writeU8 1uy
            do! fun _ -> Error expected
            do! failwith "Must not execute after failure"
        }

    Writer.run 10 write |> should equal (Error expected: Result<byte[], WError>)
    calls |> should equal 1

[<Fact>]
let ``runner reports capacity exhaustion at the failing position`` () =
    let write =
        writer {
            do! writeU8 1uy
            do! writeU16LittleEndian 0x1234us
        }

    match Writer.run 2 write with
    | Error error -> error.Pos |> should equal 1
    | Ok _ -> failwith "Expected capacity error"

[<Fact>]
let ``runner rejects negative capacity before executing the writer`` () =
    match Writer.run -1 (fun _ -> failwith "Must not execute") with
    | Error error -> error.Ctx |> should equal [ "run" ]
    | Ok _ -> failwith "Expected capacity error"

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
[<InlineData(2147483647)>]
let ``runner rejects an invalid final position`` position =
    match Writer.run 4 (fun state -> Ok((), { state with Pos = position })) with
    | Error error -> error.Pos |> should equal position
    | Ok _ -> failwith "Expected invalid-state error"

[<Fact>]
let ``backpatching restores the output end explicitly`` () =
    let write =
        writer {
            do! writeU8 0uy
            do! writeMemory (ReadOnlyMemory [| 10uy; 11uy; 12uy |])
            let! finish = getPos
            do! seek 0
            do! writeU8 (byte (finish - 1))
            do! seek finish
        }

    Writer.run 10 write |> bytes |> should equal [| 3uy; 10uy; 11uy; 12uy |]

[<Theory>]
[<InlineData(-1)>]
[<InlineData(5)>]
[<InlineData(2147483647)>]
let ``primitive writers reject invalid state without buffer mutation`` position =
    let buffer = Array.create 4 0xCCuy

    match writeU8 1uy { Buf = buffer; Pos = position } with
    | Error error -> error.Pos |> should equal position
    | Ok _ -> failwith "Expected invalid-state error"

    buffer |> should equal (Array.create 4 0xCCuy)
