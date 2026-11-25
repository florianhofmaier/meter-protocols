module Metering.Mbus.Devices.Tests.PreservationTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Devices
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Devices.Tests.DeviceTestSupport

[<Fact>]
let ``selection changes only selection state`` () =
    let before = {
        initial () with
            AccessNumber = AccessNumber.create 42uy
            Status = {
                (initial ()).Status with
                    PowerLow = true
            }
    }

    let after, _ = select address |> DeviceLogic.handleFrame before (userData [])
    after |> should equal { before with IsSelected = true }
