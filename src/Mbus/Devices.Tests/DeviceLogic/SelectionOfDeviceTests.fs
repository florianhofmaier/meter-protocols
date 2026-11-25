module Metering.Mbus.Devices.Tests.SelectionOfDeviceTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Devices
open Metering.Mbus.Devices.Tests.DeviceTestSupport
open System
open Metering.Common.Decoding.Decoders.Core

[<Fact>]
let ``matching exact and wildcard selections select and confirm`` () =
    for pattern in [ address; Array.create 8 0xFFuy; [||] ] do
        let state, action =
            DeviceLogic.handleFrame (initial ()) (userData []) (select pattern)

        state.IsSelected |> should be True
        action |> should equal SendConfirmation

[<Fact>]
let ``nonmatching selection deselects an already selected device without confirmation`` () =
    let pattern = Array.copy address
    pattern[0] <- 0x79uy

    for selected in [ false; true ] do
        let before = {
            initial () with
                IsSelected = selected
        }

        let after, action = DeviceLogic.handleFrame before (userData []) (select pattern)
        after |> should equal { before with IsSelected = false }
        action |> should equal NoAction

[<Fact>]
let ``selection requires SND UD and address 253`` () =
    for control, destination in [ 0x53uy, 5uy; 0x44uy, 0xFDuy; 0x08uy, 0xFDuy ] do
        let bytes = selectionFrame control destination address

        match decode (ReadOnlyMemory bytes) with
        | DecodeFailed(ValidationFailed _, _) -> ()
        | actual -> failwith $"Expected invalid selection envelope, got {actual}"

[<Fact>]
let ``SND NKE to 253 deselects and acknowledges only a selected device`` () =
    for selected in [ false; true ] do
        let before = {
            initial () with
                IsSelected = selected
        }

        let after, action =
            shortFrame 0x40uy 0xFDuy
            |> frame
            |> DeviceLogic.handleFrame before (userData [])

        after.IsSelected |> should be False
        action |> should equal (if selected then SendConfirmation else NoAction)

[<Fact>]
let ``reset to primary address preserves secondary selection`` () =
    let before = { initial () with IsSelected = true }

    let after, _ =
        shortFrame 0x40uy 5uy |> frame |> DeviceLogic.handleFrame before (userData [])

    after.IsSelected |> should be True
