module Mbus.Io.Tests.StreamReaderTests

open Xunit
open System.IO
open System.Threading
open Mbus.Io
open Mbus.Frames
open FsUnit.Xunit

[<Fact>]
let ``ReadAsync WhenStreamContainsConfirmation ShouldReturnConfirmationFrame`` () =
    task {
        let data = [| 0xE5uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead Frame.Confirmation -> ()
        | _ -> failwith "Expected Confirmation"
    }

[<Fact>]
let ``ReadAsync WhenStreamContainsShortFrame ShouldReturnShortFrame`` () =
    task {
        let data = [| 0x10uy; 0x40uy; 0x01uy; 0x41uy; 0x16uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead (Frame.ShortFrame _) -> ()
        | _ -> failwith "Expected ShortFrame"
    }

[<Fact>]
let ``ReadAsync WhenStreamContainsGarbageThenConfirmation ShouldReturnConfirmation`` () =
    task {
        let data = [| 0x00uy; 0xFFuy; 0xE5uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead Frame.Confirmation -> ()
        | _ -> failwith "Expected Confirmation"
    }

[<Fact>]
let ``ReadAsync WhenStreamEndsIncomplete ShouldReturnTruncatedInput`` () =
    task {
        let data = [| 0x10uy; 0x40uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.TruncatedInput bytes -> bytes.ToArray() |> should equal data
        | _ -> failwith "Expected TruncatedInput"
    }

[<Fact>]
let ``ReadAsync WhenStreamIsEmpty ShouldReturnEndOfInput`` () =
    task {
        use ms = new MemoryStream([||])
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        frame |> should equal StreamReader.EndOfInput
    }

[<Fact>]
let ``ReadAsync WhenInvalidFollowedByValid ShouldRecover`` () =
    task {
        let data = [| 0x10uy; 0x40uy; 0x01uy; 0x00uy; 0x16uy; 0xE5uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead Frame.Confirmation -> ()
        | _ -> failwith "Expected Confirmation"
    }

[<Fact>]
let ``ReadAsync WhenStreamContainsLongFrame ShouldReturnLongFrame`` () =
    task {
        let data = [| 0x68uy; 0x06uy; 0x06uy; 0x68uy; 0x53uy; 0x00uy; 0x51uy; 0x01uy; 0x7Auy; 0x01uy; 0x20uy; 0x16uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead (Frame.LongFrame _) -> ()
        | actual -> failwith $"Expected LongFrame but was %A{actual}"
    }

[<Fact>]
let ``ReadAsync WhenLongFrameChecksumInvalid ShouldSkipAndRecover`` () =
    task {
        let data = [|
            0x68uy; 0x03uy; 0x03uy; 0x68uy; 0x08uy; 0x01uy; 0x50uy; 0x00uy; 0x16uy;
            0xE5uy
        |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.FrameRead Frame.Confirmation -> ()
        | _ -> failwith "Expected Confirmation"
    }

[<Fact>]
let ``ReadAsync WhenLongFrameIncomplete ShouldReturnTruncatedInput`` () =
    task {
        let data = [| 0x68uy; 0x03uy; 0x03uy; 0x68uy; 0x08uy |]
        use ms = new MemoryStream(data)
        let reader = StreamReader.create ms 1024

        let! frame = reader CancellationToken.None

        match frame with
        | StreamReader.TruncatedInput bytes -> bytes.ToArray() |> should equal data
        | _ -> failwith "Expected TruncatedInput"
    }

