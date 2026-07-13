module Mbus.Io.Tests.StreamWriterTests

open Xunit
open FsUnit.Xunit
open System.IO
open System.Threading
open Mbus.Io
open Mbus.Frames

[<Fact>]
let ``WriteAsync WhenConfirmation ShouldWriteSingleByte`` () =
    task {
        use ms = new MemoryStream()
        let writer = StreamWriter.create ms

        do! writer Frame.Confirmation CancellationToken.None

        ms.ToArray() |> should equal [| 0xE5uy |]
    }

[<Fact>]
let ``WriteAsync WhenShortFrame ShouldWriteShortFrameBytes`` () =
    task {
        use ms = new MemoryStream()
        let writer = StreamWriter.create ms
        let sf = { CField = 0x40uy; PrmAdr = 0x01uy }

        do! writer (Frame.ShortFrame sf) CancellationToken.None

        let expected = [| 0x10uy; 0x40uy; 0x01uy; 0x41uy; 0x16uy |]
        ms.ToArray() |> should equal expected
    }

[<Fact>]
let ``WriteAsync WhenLongFrameCiOnly ShouldWriteCorrectBytes`` () =
    task {
        use ms = new MemoryStream()
        let writer = StreamWriter.create ms
        let tpl = Tpl.CiOnly TplCiOnlyFunc.AplSelect
        let apl = Apl.RspUdData { DataRecords = []; MfrSpecificData = None; IsMoreDataInNextTelegram = false }
        let lf = { CField = 0x53uy; PrmAdr = 0x01uy; Tpl = tpl; Apl = apl }

        do! writer (Frame.LongFrame lf) CancellationToken.None

        let expected = [| 0x68uy; 0x03uy; 0x03uy; 0x68uy; 0x53uy; 0x01uy; 0x50uy; 0xA4uy; 0x16uy |]
        ms.ToArray() |> should equal expected
    }