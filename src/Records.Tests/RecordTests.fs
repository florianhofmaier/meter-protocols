module Mbus.Records.Tests.RspRecordTests

open System
open Mbus
open Mbus.BaseParsers.Core
open Mbus.Records
open Mbus.Records.DataInfoBlocks
open Mbus.Records.ValueInfoBlocks
open Xunit
open FsUnit.Xunit

let PRec buf =
    match Record.Parser.parseRspRec { Off = 0; Buf = ReadOnlyMemory<uint8>(buf) } with
    | Ok (r, _) -> r
    | Error e -> failwithf $"Unexpected: %A{e}"

[<Fact>]
let ``parse idle filler`` () =
    let buf = [| 0x2Fuy |]
    match PRec buf with
    | SpecialFunction r -> r |> should equal IdleFiller
    | other -> failwithf $"Expected IdleFiller, got {other}"

[<Fact>]
let ``parse mfr specific record`` () =
    let buf = [| 0x0Fuy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
    match PRec buf with
    | SpecialFunction (MfrData mem) -> mem.Span.ToArray() |> should equal buf[1..]
    | other -> failwithf $"Expected MfrData, got {other}"

[<Fact>]
let ``parse mfr specific record with more follows`` () =
    let buf = [| 0x1Fuy; 0x01uy; 0x02uy; 0x03uy; 0x04uy |]
    match PRec buf with
    | SpecialFunction (MfrDataMoreFollows mem) -> mem.Span.ToArray() |> should equal buf[1..]
    | other -> failwithf $"Expected MfrDataMoreFollows, got {other}"

[<Fact>]
let ``parse invalid special function code`` () =
    let buf = [| 0x3Fuy |]
    match Record.Parser.parseRspRec { Off = 0; Buf = ReadOnlyMemory<uint8>(buf) } with
    | Error e -> e |> should equal { Pos = 0; Msg = "invalid special function code: 0x3F"; Ctx = [ "special function record" ] }
    | Ok (r, _) -> failwithf $"Expected error, got {r}"

[<Fact>]
let ``parse data record`` () =
    let buf = [| 0x04uy; 0x13uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy; |]
    match PRec buf with
    | Data r ->
        r.Value |> should equal (Int32 0x12345678)
        r.Fn |> should equal MbusFunctionField.InstValue
        r.StNum |> should equal StorageNumber.zero
        r.Tariff |> should equal Tariff.zero
        r.SubUnit |> should equal SubUnit.zero
        r.Vib |> should equal (RspVib.Normal { Def = { Val = MbusValueType.Volume; Unit = MbusUnit.CubicMeters; Scaler = 1e-3m }; Ext = [ ]; Codes = [] })
    | other -> failwithf $"Expected DataRecord, got {other}"