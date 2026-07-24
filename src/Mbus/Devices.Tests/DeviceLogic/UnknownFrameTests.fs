module Devices.Tests.UnknownFrameTests

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
let ``handleFrame when unknown frame should return NoAction`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.Confirmation

    let newState, action = DeviceLogic.handleFrame state testUserData frame

    action |> should equal NoAction
    newState |> should equal state

[<Fact>]
let ``handleFrame when ShortFrame with non-ReqUd2 CField should return NoAction`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.ShortFrame { CField = 0x40uy; PrmAdr = 0x05uy }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    action |> should equal NoAction

[<Fact>]
let ``handleFrame when LongFrame with Command CI should return NoAction`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.LongFrame {
        CField = 0x53uy
        PrmAdr = 0x05uy
        Tpl = Tpl.CiOnly TplCiOnlyFunc.Command
        Apl = Apl.SndUdData [ ]
    }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    action |> should equal NoAction

