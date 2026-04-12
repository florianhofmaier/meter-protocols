module Devices.Tests.PreservationTests

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
let ``handleFrame should preserve primary address`` () =
    let state = createTestState 0x42uy testAddress
    let frame = Frame.Confirmation

    let newState, _ = DeviceLogic.handleFrame state testUserData frame

    newState.PrimaryAddress |> should equal 0x42uy

[<Fact>]
let ``handleFrame should preserve secondary address`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.Confirmation

    let newState, _ = DeviceLogic.handleFrame state testUserData frame

    newState.SecondaryAddress |> should equal testAddress

[<Fact>]
let ``handleFrame ReqUd2 should not change IsSelected state`` () =
    let selectedState = { createTestState 0x05uy testAddress with IsSelected = true }
    let frame = Frame.ShortFrame { CField = 0x5Buy; PrmAdr = 0x05uy }

    let newState, _ = DeviceLogic.handleFrame selectedState testUserData frame

    newState.IsSelected |> should be True

[<Fact>]
let ``handleFrame SelectDevice should only change IsSelected state`` () =
    let state = createTestState 0x42uy testAddress
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    let frame = Frame.LongFrame {
        CField = 0x53uy
        PrmAdr = 0xFDuy
        Tpl = Tpl.CiOnly TplCiOnlyFunc.DevSelect
        Apl = Apl.SelectedDevice selection
    }

    let newState, _ = DeviceLogic.handleFrame state testUserData frame

    newState.PrimaryAddress |> should equal state.PrimaryAddress
    newState.SecondaryAddress |> should equal state.SecondaryAddress
    newState.AccessNumber |> should equal state.AccessNumber
    newState.IsSelected |> should be True

