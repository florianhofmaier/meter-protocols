module Mbus.Records.Tests.RspVibTests

open Mbus
open Mbus.Records.ValueInfoBlocks
open Mbus.Records.ValueInfoBlocks.VifDef
open FsUnit.Xunit
open System
open Xunit

let PVib buf =
    match Vib.Parser.parseRsp { Off = 0; Buf = ReadOnlyMemory<uint8>(buf) } with
    | Ok (v, _) -> v
    | Error e -> failwithf $"Unexpected: %A{e}"

[<Fact>]
let ``parse primary vif`` () =
    let buf = [| 0x14uy |]
    let def = { Val = Volume; Unit = CubicMeters; Scaler = 1e-2m }
    PVib buf
    |> should equal (RspVib.Normal { Def = def; Ext = [ ]; Codes = [] })

[<Fact>]
let ``parse primary vif with invalid value`` () =
    let buf = [| 0x6Fuy |]
    match PVib buf with
    | RspVib.Invalid inv -> inv.Span.ToArray() |> should equal buf
    | other -> failwithf $"Expected Invalid, got %A{other}"

[<Fact>]
let ``parse primary vif with orthogonal extensions`` () =
    let buf = [| 0x94uy; 0x81uy; 0x82uy; 0x83uy; 0x14uy |]
    let def = { Val = Volume; Unit = CubicMeters; Scaler = 1e-2m }
    PVib buf
    |> should equal (RspVib.Normal { Def = def
                                     Ext = [ InvalidCombVifExt 0x81uy
                                             InvalidCombVifExt 0x82uy
                                             InvalidCombVifExt 0x83uy
                                             ValidCombVifExt MbusValueTypeExtension.RelativeDeviation ]
                                     Codes = [] })

[<Fact>]
let ``parse primary vif with invalid value and orthogonal extension`` () =
    let buf = [| 0xEFuy; 0x82uy; 0x42uy;|]
    match PVib buf with
    | RspVib.Invalid inv -> inv.Span.ToArray() |> should equal buf
    | other -> failwithf $"Expected Invalid, got %A{other}"

[<Fact>]
let ``parse vif of first extension`` () =
    let buf = [| 0xFBuy; 0x2Buy; |]
    let def = { Val = PhaseUToI; Unit = Degrees; Scaler = 0.1m }
    PVib buf |> should equal (RspVib.Normal { Def = def; Ext = [ ]; Codes = [] })

[<Fact>]
let ``parse vif of first extension when value is invalid``() =
    let buf = [| 0xFBuy; 0x06uy |]
    match PVib buf with
    | RspVib.Invalid inv -> inv.Span.ToArray() |> should equal buf
    | other -> failwithf $"Expected Invalid, got %A{other}"

[<Fact>]
let ``parse vif of second extension`` () =
    let buf = [| 0xFDuy; 0x17uy; |]
    let def = { Val = ErrorFlags; Unit = NoUnit; Scaler = 1m }
    PVib buf |> should equal (RspVib.Normal { Def = def; Ext = [ ]; Codes = [] })

[<Fact>]
let ``parse text vif`` () =
    let buf = [| 0x7Cuy; 0x0Auy; 33uy; 101uy; 114uy; 101uy; 104uy; 84uy; 32uy; 121uy; 101uy; 72uy |]
    PVib buf |> should equal (RspVib.Text "Hey There!")
