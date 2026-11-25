module Metering.Mbus.Devices.Tests.RequestTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Devices
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Devices.Tests.DeviceTestSupport

[<Fact>]
let ``class two requests use the configured address or selected secondary address`` () =
    for selected, destination, responds in
        [
            false, 5uy, true
            true, 5uy, true
            false, 6uy, false
            false, 0xFDuy, false
            true, 0xFDuy, true
        ] do
        let before = {
            initial () with
                IsSelected = selected
        }

        let after, action =
            shortFrame 0x5Buy destination
            |> frame
            |> DeviceLogic.handleFrame before (userData [])

        if responds then
            AccessNumber.value after.AccessNumber |> should equal 1uy
        else
            after |> should equal before

        match action with
        | SendUserData response when responds ->
            IdNumber.value response.MeterAddress.IdNum |> should equal 12345678u
            response.UserData |> Seq.isEmpty |> should be True
            response.Status |> should equal before.Status
        | NoAction when not responds -> ()
        | actual -> failwith $"Unexpected action {actual}"

[<Fact>]
let ``unconfigured device responds only to its own address before selection`` () =
    let state = DeviceLogic.initialState PrimaryAddress.Unconfigured identity

    let _, action =
        shortFrame 0x5Buy 0uy |> frame |> DeviceLogic.handleFrame state (userData [])

    match action with
    | SendUserData _ -> ()
    | actual -> failwith $"Expected response, got {actual}"

[<Fact>]
let ``response preserves supplied records and status`` () =
    let raw =
        run (Decoder.parseValue RecordRaw.parse) [| 0x04uy; 0x13uy; 0x39uy; 0x30uy; 0uy; 0uy |]

    let record =
        match RecordRsp.validate raw with
        | Passed(RecordRsp.Data record, _) -> record
        | actual -> failwith $"Expected valid test record, got {actual}"

    let state = {
        initial () with
            Status = {
                (initial ()).Status with
                    PowerLow = true
            }
    }

    let _, action =
        shortFrame 0x5Buy 5uy
        |> frame
        |> DeviceLogic.handleFrame state (userData [ record ])

    match action with
    | SendUserData response ->
        response.UserData |> Seq.toList |> should equal [ record ]
        response.Status |> should equal state.Status
    | actual -> failwith $"Expected user-data response, got {actual}"
