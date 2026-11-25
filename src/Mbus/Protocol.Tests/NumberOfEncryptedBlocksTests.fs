module Metering.Mbus.Protocol.Tests.NumberOfEncryptedBlocksTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Frames.TransportLayer

[<Fact>]
let ``zero N bits map to no encrypted data`` () =
    NumberOfEncryptedBlocks.map 0x0000us
    |> NumberOfEncryptedBlocks.value
    |> should equal 0

[<Fact>]
let ``one N bit value maps to one fixed encrypted block`` () =
    NumberOfEncryptedBlocks.map 0x0010us
    |> NumberOfEncryptedBlocks.value
    |> should equal 1

[<Fact>]
let ``fourteen N bit value maps to fourteen fixed encrypted blocks`` () =
    NumberOfEncryptedBlocks.map 0x00E0us
    |> NumberOfEncryptedBlocks.value
    |> should equal 14

[<Fact>]
let ``all N bits set maps to all remaining data encrypted`` () =
    NumberOfEncryptedBlocks.map 0x00F0us
    |> NumberOfEncryptedBlocks.value
    |> should equal 15

[<Fact>]
let ``all remaining data indicator is obtained from configuration bits`` () =
    NumberOfEncryptedBlocks.map 0x05F0us
    |> NumberOfEncryptedBlocks.value
    |> should equal 15

[<Fact>]
let ``block count mapping ignores other configuration bits`` () =
    NumberOfEncryptedBlocks.map 0xFFEFus
    |> NumberOfEncryptedBlocks.value
    |> should equal 14
