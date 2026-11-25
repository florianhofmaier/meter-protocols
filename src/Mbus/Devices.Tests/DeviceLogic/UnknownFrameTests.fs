module Metering.Mbus.Devices.Tests.UnknownFrameTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Devices
open Metering.Mbus.Devices.Tests.DeviceTestSupport
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Messages

[<Fact>]
let ``class one requests acknowledge when there are no alarms`` () =
    let before = { initial () with IsSelected = true }

    let input =
        Message.PrimaryStationMessage(
            PrimaryStationMessage.RequestAlarms {
                Fcb = false
                Address = AField.Configured(PrimAdr.tryCreate 5uy |> Option.get)
            }
        )

    let after, action = DeviceLogic.handleFrame before (userData []) input
    after.IsSelected |> should equal before.IsSelected
    after.AccessNumber |> should equal before.AccessNumber
    after.PrimaryLink |> should equal (Confirmed(false, false))
    action |> should equal SendConfirmation
