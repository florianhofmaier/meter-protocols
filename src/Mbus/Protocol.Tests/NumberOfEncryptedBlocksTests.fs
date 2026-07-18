module Metering.Mbus.Protocol.Tests.NumberOfEncryptedBlocksTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Frames.Transport

[<Fact>]
let ``zero N bits map to no encrypted data`` () =
    EncryptedLengthIndicator.map 0x0000us
    |> should equal NoEncryptedData

[<Fact>]
let ``one N bit value maps to one fixed encrypted block`` () =
    match EncryptedLengthIndicator.map 0x0010us with
    | FixedEncryptedBlocks blocks ->
        EncryptedBlockCount.value blocks |> should equal 1

    | actual ->
        failwith $"Expected one fixed encrypted block, got %A{actual}"

[<Fact>]
let ``fourteen N bit value maps to fourteen fixed encrypted blocks`` () =
    match EncryptedLengthIndicator.map 0x00E0us with
    | FixedEncryptedBlocks blocks ->
        EncryptedBlockCount.value blocks |> should equal 14

    | actual ->
        failwith $"Expected fourteen fixed encrypted blocks, got %A{actual}"

[<Fact>]
let ``all N bits set maps to all remaining data encrypted`` () =
    EncryptedLengthIndicator.map 0x00F0us
    |> should equal AllRemainingDataEncrypted

[<Fact>]
let ``fixed encrypted block count cannot represent sentinel values`` () =
    EncryptedBlockCount.tryCreate 0 |> should equal None
    EncryptedBlockCount.tryCreate 15 |> should equal None

[<Fact>]
let ``fixed encrypted block count rejects values above four-bit range`` () =
    EncryptedBlockCount.tryCreate 16
    |> should equal None
