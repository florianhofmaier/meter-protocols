module Metering.Mbus.Devices.Tests.InitialStateTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Devices.Tests.DeviceTestSupport

[<Fact>]
let ``initial state is unselected with the configured identity and zero access number`` () =
    let state = initial ()
    state.IsSelected |> should be False
    state.PrimaryAddress |> should equal primaryAddress
    state.SecondaryAddress |> should equal identity
    AccessNumber.value state.AccessNumber |> should equal 0uy
    state.Status.ApplicationError |> should equal ApplicationError.NoError
