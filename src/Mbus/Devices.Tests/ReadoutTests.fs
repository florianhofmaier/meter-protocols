module Metering.Mbus.Devices.Tests.ReadoutTests

open System
open System.IO
open Xunit
open Metering.Mbus.Devices
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.ValueInfoBlocks
open Metering.Mbus.Protocol.Messages
open Metering.Common.BaseWriters.Core
open Metering.Common.Decoding.Decoders.Core
open Metering.Mbus.Devices.Tests.DeviceTestSupport

let private record value =
    match UserData.decode (ReadOnlyMemory [| 4uy; 0x13uy; 0uy; 0uy; 0uy; 0uy |]) with
    | Decoded(data, _) -> {
        (RspUdData.records data).Head with
            Value = Value.Int32 value
      }
    | _ -> failwith "Invalid fixture"

let private data count offset =
    [ for i in 0 .. count - 1 -> record (offset + i) ] |> userData

let private step state data fcb address =
    DeviceLogic.handleFrame
        state
        data
        (Message.PrimaryStationMessage(
            PrimaryStationMessage.RequestUserData { Address = address; Fcb = fcb }
        ))

let private primary = AField.Configured(PrimAdr.tryCreate 5uy |> Option.get)

let private output =
    function
    | SendUserData response -> response
    | other -> failwith $"Expected data, got {other}"

let private records (response: ResponseUserData) =
    response.UserData
    |> Seq.filter (fun r ->
        match r.Vib with
        | VibRsp.Normal vib ->
            vib.Def.Val <> Metering.Mbus.MbusValueType.CurrentSelectedApplication
        | _ -> true)
    |> Seq.toList

let private assertRecords expected actual =
    let encode records =
        UserData.encode records |> Result.defaultWith (fun e -> failwith e.Msg)

    Assert.Equal<byte>(encode expected, encode actual)

let private prepared data =
    Readout.prepare false data |> Result.defaultWith failwith

[<Fact>]
let ``single block fits exactly without continuation overhead`` () =
    let blocks = prepared (data 40 0)
    Assert.Single(blocks) |> ignore
    Assert.False(RspUdData.moreFollows blocks.Head)
    Assert.Equal(40, (RspUdData.records blocks.Head).Length)

[<Fact>]
let ``multiple blocks fit preserve order and carry identification and continuation`` () =
    let source = data 100 0
    let blocks = prepared source
    Assert.Equal(3, blocks.Length)
    let restored = ResizeArray<DataRecordRsp>()

    for index, block in List.indexed blocks do
        let encoded =
            Writer.run 240 (RspUdData.write block)
            |> Result.defaultWith (fun e -> failwith e.Msg)

        Assert.InRange(encoded.Length, 1, 240)
        Assert.Equal(index < blocks.Length - 1, RspUdData.moreFollows block)
        assertRecords [ Readout.blockIdentification index ] [ (RspUdData.records block).Head ]
        restored.AddRange((RspUdData.records block).Tail)

    assertRecords (RspUdData.records source) (restored)

[<Fact>]
let ``oversized individual record is rejected before any response`` () =
    let oversized = {
        record 1 with
            Vib = VibRsp.Text(String.replicate 255 "x")
    }

    Assert.True((Readout.prepare false (userData [ oversized ])).IsError)

    Assert.Throws<InvalidDataException>(fun () ->
        step (initial ()) (userData [ oversized ]) false primary |> ignore)
    |> ignore

[<Fact>]
let ``manufacturer data remains at end of the final block`` () =
    let mfr = MfrSpecificData.ofBytes (ReadOnlyMemory(Array.create 40 0xAAuy))

    let blocks =
        prepared (RspUdData.create (RspUdData.records (data 100 0)) (Some mfr) false)

    Assert.Equal(Some mfr, RspUdData.mfrData (List.last blocks))

    for block in blocks |> List.take (blocks.Length - 1) do
        Assert.True((RspUdData.mfrData block).IsNone)

[<Fact>]
let ``FCB repeats a fixed block and updates wait until the next full readout`` () =
    let oldData, newData = data 100 0, data 1 999
    let state1, action1 = step (initial ()) oldData false primary
    let first = output action1
    let repeatState, repeated = step state1 newData false primary
    assertRecords (first.UserData) ((output repeated).UserData)
    Assert.Equal(2uy, AccessNumber.value repeatState.AccessNumber)
    let state2, action2 = step repeatState newData true primary
    let state3, action3 = step state2 newData false primary
    let all = [ first; output action2; output action3 ] |> List.collect records
    assertRecords (RspUdData.records oldData) (all)
    Assert.False((output action3).MoreFollows)
    let _, finalRepeat = step state3 newData false primary
    assertRecords ((output action3).UserData) ((output finalRepeat).UserData)
    let _, next = step state3 newData true primary
    assertRecords (RspUdData.records newData) ((output next).UserData)

[<Fact>]
let ``primary secondary histories are independent and diagnosis shares primary`` () =
    let source = data 100 0
    let before = { initial () with IsSelected = true }
    let state1, firstPrimary = step before source false primary
    let state2, firstSecondary = step state1 source false AField.SelectionOfDevice
    assertRecords ((output firstPrimary).UserData) ((output firstSecondary).UserData)
    let state3, nextPrimary = step state2 source true AField.Diagnosis
    let _, secondaryRepeat = step state3 source false AField.SelectionOfDevice
    assertRecords ((output firstSecondary).UserData) ((output secondaryRepeat).UserData)
    Assert.NotEqual<DataRecordRsp list>(records (output firstPrimary), records (output nextPrimary))

[<Fact>]
let ``link reset normalizes FCB and application reset restarts interrupted readout`` () =
    let source = data 100 0
    let before, _ = step (initial ()) source false primary

    let reset =
        Message.PrimaryStationMessage(PrimaryStationMessage.LinkLayerReset { Address = primary })

    let interrupted, ack = DeviceLogic.handleFrame before source reset
    Assert.Equal(SendConfirmation, ack)
    let _, stopped = step interrupted source true primary
    Assert.Equal(NoAction, stopped)

    let applicationReset =
        Message.PrimaryStationMessage(
            PrimaryStationMessage.ApplicationResetOrSelect {
                Address = primary
                Selection = ApplicationResetOrSelect.ApplicationReset
            }
        )

    let restarted, ack = DeviceLogic.handleFrame interrupted source applicationReset
    Assert.Equal(SendConfirmation, ack)
    let _, response = step restarted source true primary

    assertRecords
        ((output (snd (step (initial ()) source false primary))).UserData)
        ((output response).UserData)

    let normalized, _ = DeviceLogic.handleFrame (initial ()) source reset
    Assert.Equal(NoAction, snd (step normalized source false primary))

    Assert.IsType<ResponseUserData>(output (snd (step normalized source true primary)))
    |> ignore

[<Fact>]
let ``access number wraps while a repeated block remains unchanged`` () =
    let source = data 1 0

    let before = {
        initial () with
            AccessNumber = AccessNumber.create 255uy
    }

    let after, _ = step before source false primary
    Assert.Equal(0uy, AccessNumber.value after.AccessNumber)

[<Fact>]
let ``block selection out of range falls back to block zero`` () =
    let source = data 100 0

    let select block =
        Message.PrimaryStationMessage(
            PrimaryStationMessage.ApplicationResetOrSelect {
                Address = primary
                Selection =
                    ApplicationResetOrSelect.ApplicationSelect(ReadOnlyMemory [| byte block |])
            }
        )

    let selected, _ = DeviceLogic.handleFrame (initial ()) source (select 1)
    let _, selectedResponse = step selected source false primary
    let expected = (prepared source)[1] |> RspUdData.records
    assertRecords (expected) ((output selectedResponse).UserData)
    let selected, _ = DeviceLogic.handleFrame (initial ()) source (select 14)
    let _, selectedResponse = step selected source false primary

    assertRecords ((prepared source).Head |> RspUdData.records) ((output selectedResponse).UserData)
