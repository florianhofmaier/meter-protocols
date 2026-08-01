module Metering.Mbus.Protocol.Tests.Mode5SecurityContextTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Tests.TestSupport

let private validKeyBytes =
    [|
        0x00uy; 0x01uy; 0x02uy; 0x03uy
        0x04uy; 0x05uy; 0x06uy; 0x07uy
        0x08uy; 0x09uy; 0x0Auy; 0x0Buy
        0x0Cuy; 0x0Duy; 0x0Euy; 0x0Fuy
    |]

[<Fact>]
let ``sixteen byte key creates mode five key and context`` () =
    match Mode5Key.create (memory validKeyBytes) with
    | Ok key ->
        Mode5Key.value key
        |> fun bytes -> bytes.ToArray()
        |> should equal validKeyBytes

        match SecurityContext.mode5 key with
        | SecurityContext.Mode5 context ->
            context
            |> Mode5SecurityContext.key
            |> Mode5Key.value
            |> fun bytes -> bytes.ToArray()
            |> should equal validKeyBytes

            context
            |> Mode5SecurityContext.keyBytes
            |> fun bytes -> bytes.ToArray()
            |> should equal validKeyBytes

        | actual ->
            failwith $"Expected Mode5 security context, got %A{actual}"

    | Error error ->
        failwith $"Expected valid Mode 5 key, got %A{error}"

[<Theory>]
[<InlineData(0)>]
[<InlineData(15)>]
[<InlineData(17)>]
[<InlineData(24)>]
[<InlineData(32)>]
let ``invalid mode five key lengths return creation error`` length =
    let bytes =
        Array.zeroCreate<byte> length
        |> memory

    match Mode5Key.create bytes with
    | Error (InvalidLength actualLength) ->
        actualLength |> should equal length

    | actual ->
        failwith $"Expected invalid Mode 5 key length {length}, got %A{actual}"
