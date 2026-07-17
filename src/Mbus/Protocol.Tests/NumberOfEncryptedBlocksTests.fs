module Metering.Mbus.Protocol.Tests.NumberOfEncryptedBlocksTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Frames.Transport

[<Fact>]
let ``block count fifteen can be represented`` () =
    match NumberOfEncryptedBlocks.tryCreate 15 with
    | Some blocks ->
        NumberOfEncryptedBlocks.value blocks |> should equal 15

    | None ->
        failwith "Expected block count 15 to be valid"

[<Fact>]
let ``block count above four-bit range is rejected`` () =
    NumberOfEncryptedBlocks.tryCreate 16
    |> should equal None

