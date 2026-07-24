module Devices.Tests.RequestTests

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
let ``handleFrame when ReqUd2 to primary address should respond with user data`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.ShortFrame { CField = 0x5Buy; PrmAdr = 0x05uy }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    match action with
    | SendFrame (Frame.LongFrame {
        CField = 0x08uy
        PrmAdr = 0x05uy
        Tpl = Tpl.Long {
            Func = TplLongFunc.Rsp
            Ala = {
                IdNumber = 12345678
                Mfr = "GWF"
                Version = 60
                DeviceType = MbusDeviceType.WaterMeter }
            Acc = 0uy
            Status = {
                ApplicationError = MbusApplicationError.NoError
                PowerLow = false
                TemporaryError = false
                PermanentError = false }
            Cnf = 0us }
        Apl = Apl.RspUdData { DataRecords = []; MfrSpecificData = None; IsMoreDataInNextTelegram = false }
    }) -> ()
    | _ -> failwith "Expected SendFrame with LongFrame containing RSP_UD response"

[<Fact>]
let ``handleFrame when ReqUd2 to different primary address should not respond`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.ShortFrame { CField = 0x5Buy; PrmAdr = 0x06uy }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    action |> should equal NoAction

[<Fact>]
let ``handleFrame when ReqUd2 to 0xFD and device is selected should respond`` () =
    let state = { createTestState 0x05uy testAddress with IsSelected = true }
    let frame = Frame.ShortFrame { CField = 0x5Buy; PrmAdr = 0xFDuy }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    match action with
    | SendFrame (Frame.LongFrame _) -> ()
    | _ -> failwith "Expected SendFrame with LongFrame"

[<Fact>]
let ``handleFrame when ReqUd2 to 0xFD and device is not selected should not respond`` () =
    let state = createTestState 0x05uy testAddress
    let frame = Frame.ShortFrame { CField = 0x5Buy; PrmAdr = 0xFDuy }

    let _newState, action = DeviceLogic.handleFrame state testUserData frame

    action |> should equal NoAction
