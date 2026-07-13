module Devices.Tests.SelectionTests

open Mbus
open Mbus.Devices
open Mbus.Frames
open Xunit
open FsUnit.Xunit

let createAddress id mfr version deviceType =
    match MbusAddress.create id mfr version deviceType with
    | Ok addr -> addr
    | Error msg -> failwith $"Failed to create address: {msg}"

let createTestState prmAdr secAdr =
    DeviceLogic.initialState prmAdr secAdr

let testAddress = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
let testUserData = []

[<Fact>]
let ``handleFrame when SelectDevice matches should select device and send confirmation`` () =
    let state = createTestState 0x01uy testAddress
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    let frame = Frame.LongFrame {
        CField = 0x53uy
        PrmAdr = 0xFDuy
        Tpl = Tpl.CiOnly TplCiOnlyFunc.DevSelect
        Apl = Apl.SelectedDevice selection
    }

    let newState, action = DeviceLogic.handleFrame state testUserData frame

    newState.IsSelected |> should be True
    action |> should equal (SendFrame Frame.Confirmation)

[<Fact>]
let ``handleFrame when SelectDevice does not match should not select and send no action`` () =
    let state = createTestState 0x01uy testAddress
    let selection = [| 0x79uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    let frame = Frame.LongFrame {
        CField = 0x53uy
        PrmAdr = 0xFDuy
        Tpl = Tpl.CiOnly TplCiOnlyFunc.DevSelect
        Apl = Apl.SelectedDevice selection
    }

    let newState, action = DeviceLogic.handleFrame state testUserData frame

    newState.IsSelected |> should be False
    action |> should equal NoAction

[<Fact>]
let ``handleFrame when SelectDevice with wildcard matches should select device`` () =
    let state = createTestState 0x01uy testAddress
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    let frame = Frame.LongFrame {
        CField = 0x53uy
        PrmAdr = 0xFDuy
        Tpl = Tpl.CiOnly TplCiOnlyFunc.DevSelect
        Apl = Apl.SelectedDevice selection
    }

    let newState, action = DeviceLogic.handleFrame state testUserData frame

    newState.IsSelected |> should be True
    action |> should equal (SendFrame Frame.Confirmation)

