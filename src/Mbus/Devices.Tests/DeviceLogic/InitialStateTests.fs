module Devices.Tests.InitialStateTests

open Mbus
open Mbus.Devices
open Xunit
open FsUnit.Xunit

let createAddress id mfr version deviceType =
    match MbusAddress.create id mfr version deviceType with
    | Ok addr -> addr
    | Error msg -> failwith $"Failed to create address: {msg}"

let testAddress = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter

[<Fact>]
let ``initialState should create state with not selected`` () =
    let state = DeviceLogic.initialState 0x01uy testAddress
    state.IsSelected |> should be False

[<Fact>]
let ``initialState should set primary address`` () =
    let state = DeviceLogic.initialState 0x42uy testAddress
    state.PrimaryAddress |> should equal 0x42uy

[<Fact>]
let ``initialState should set secondary address`` () =
    let state = DeviceLogic.initialState 0x01uy testAddress
    state.SecondaryAddress |> should equal testAddress

[<Fact>]
let ``initialState should set access number to zero`` () =
    let state = DeviceLogic.initialState 0x01uy testAddress
    state.AccessNumber |> should equal 0uy

