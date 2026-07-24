module Devices.Tests.DeviceRuntimeTests

open System
open Mbus.Devices
open System.IO
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit

let createAddress id mfr version deviceType =
    match MbusAddress.create id mfr version deviceType with
    | Ok addr -> addr
    | Error msg -> failwith $"Failed to create address: {msg}"

let testAddress = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter

[<Fact>]
let ``runAsync should respond to REQ_UD2 on primary address`` () =
    task {
        // Arrange: Create a stream with REQ_UD2 frame
        let reqUd2Frame = [| 0x10uy; 0x5Buy; 0x05uy; 0x60uy; 0x16uy |]
        use ms = new MemoryStream()
        do! ms.WriteAsync(ReadOnlyMemory(reqUd2Frame))
        ms.Position <- 0L

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress

        // Act
        do! DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None

        // Assert: Check response was written
        ms.Position <- reqUd2Frame.Length
        let responseLength = int (ms.Length - int64 reqUd2Frame.Length)
        responseLength |> should be (greaterThan 0)
    }

[<Fact>]
let ``runAsync should throw OperationCanceledException when canceled on blocking stream`` () =
    task {
        use ms =
            { new Stream() with
                override _.CanRead = true
                override _.CanSeek = false
                override _.CanWrite = true
                override _.Length = 0L
                override _.Position
                    with get () = 0L
                    and set _ = ()

                override _.Flush() = ()
                override _.Seek(_, _) = 0L
                override _.SetLength(_) = ()
                override _.Write(_, _, _) = ()
                override _.Read(_, _, _) = 0

                override _.ReadAsync(buffer: Memory<byte>, ct: CancellationToken) =
                    ValueTask<int>(
                        task {
                            do! Task.Delay(Timeout.Infinite, ct)
                            return 0
                        })
            }

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress
        use cts = new CancellationTokenSource()

        let runTask = DeviceRuntime.runAsync ms getUserData initialState cts.Token

        do! Task.Delay(10)
        cts.Cancel()

        let! ex = Record.ExceptionAsync(fun () -> runTask)

        match ex with
        | :? OperationCanceledException -> ()
        | _ -> failwith $"Expected OperationCanceledException but got %A{ex}"
    }

[<Fact>]
let ``runAsync should throw InvalidDataException on truncated input`` () =
    task {
        // Arrange: Start of a long frame but ends prematurely
        let truncatedFrame = [| 0x68uy; 0x0Buy; 0x0Buy; 0x68uy; 0x53uy; 0xFDuy |]
        use ms = new MemoryStream(truncatedFrame)

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress

        // Act
        let! ex = Record.ExceptionAsync(fun () -> DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None)

        // Assert
        match ex with
        | :? InvalidDataException -> ex.Message |> should startWith "Truncated frame at end of stream"
        | _ -> failwith $"Expected InvalidDataException but got %A{ex}"
    }

[<Fact>]
let ``runAsync should not respond to REQ_UD2 on different address`` () =
    task {
        // Arrange: Create a stream with REQ_UD2 to different address
        let reqUd2Frame = [| 0x10uy; 0x5Buy; 0x06uy; 0x61uy; 0x16uy |] // Address 0x06
        use ms = new MemoryStream()
        do! ms.WriteAsync(ReadOnlyMemory(reqUd2Frame))
        ms.Position <- 0L

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress // Device has address 0x05

        // Act
        do! DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None

        // Assert: Check no response was written
        ms.Position <- reqUd2Frame.Length
        let responseLength = int (ms.Length - int64 reqUd2Frame.Length)
        responseLength |> should equal 0
    }

[<Fact>]
let ``runAsync should handle device selection and subsequent request`` () =
    task {
        // Arrange: Selection frame + REQ_UD2 to 0xFD
        let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
        let selectionFrame =
            [| 0x68uy; 0x0Buy; 0x0Buy; 0x68uy; // Header
               0x53uy; 0xFDuy; 0x52uy |] // C-Field, Address, CI
            |> Array.append <| selection // Selection data
            |> Array.append <| [| 0x00uy; 0x16uy |] // Checksum placeholder + Stop

        // Calculate actual checksum
        let checksumData = selectionFrame[4..14]
        let checksum = Array.fold (fun acc b -> (acc + int b) &&& 0xFF) 0 checksumData |> byte
        selectionFrame[15] <- checksum

        let reqUd2ToFD = [| 0x10uy; 0x5Buy; 0xFDuy; 0x58uy; 0x16uy |]
        let fullFrame = Array.append selectionFrame reqUd2ToFD

        use ms = new MemoryStream()
        do! ms.WriteAsync(ReadOnlyMemory(fullFrame))
        ms.Position <- 0L

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress

        // Act
        do! DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None

        // Assert: Should have confirmation + response
        ms.Position <- fullFrame.Length
        let responseLength = int (ms.Length - int64 fullFrame.Length)
        responseLength |> should be (greaterThan 1)
    }

[<Fact>]
let ``runAsync should call getUserData for each request`` () =
    task {
        // Arrange
        let reqUd2Frame = [| 0x10uy; 0x5Buy; 0x05uy; 0x60uy; 0x16uy |]
        use ms = new MemoryStream()
        do! ms.WriteAsync(ReadOnlyMemory(reqUd2Frame))
        ms.Position <- 0L

        let mutable callCount = 0
        let getUserData () =
            callCount <- callCount + 1
            []

        let initialState = DeviceLogic.initialState 0x05uy testAddress

        // Act
        do! DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None

        // Assert
        callCount |> should be (greaterThan 0)
    }

[<Fact>]
let ``runAsync should maintain state across multiple frames`` () =
    task {
        // Arrange: Selection + two requests
        let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
        let selectionFrame =
            [| 0x68uy; 0x0Buy; 0x0Buy; 0x68uy; 0x53uy; 0xFDuy; 0x52uy |]
            |> Array.append <| selection
            |> Array.append <| [| 0x00uy; 0x16uy |]

        let checksumData = selectionFrame[4..14]
        let checksum = Array.fold (fun acc b -> (acc + int b) &&& 0xFF) 0 checksumData |> byte
        selectionFrame[15] <- checksum

        let reqUd2 = [| 0x10uy; 0x5Buy; 0xFDuy; 0x58uy; 0x16uy |]
        let fullFrame =
            selectionFrame
            |> Array.append <| reqUd2
            |> Array.append <| reqUd2 // Second request

        use ms = new MemoryStream()
        do! ms.WriteAsync(ReadOnlyMemory(fullFrame))
        ms.Position <- 0L

        let getUserData () = []
        let initialState = DeviceLogic.initialState 0x05uy testAddress

        // Act
        do! DeviceRuntime.runAsync ms getUserData initialState CancellationToken.None

        // Assert: Should respond to both requests (device stays selected)
        ms.Position <- fullFrame.Length
        let responseLength = int (ms.Length - int64 fullFrame.Length)
        // Should have: confirmation + response1 + response2 (at least 3 frames)
        responseLength |> should be (greaterThan 10)
    }

