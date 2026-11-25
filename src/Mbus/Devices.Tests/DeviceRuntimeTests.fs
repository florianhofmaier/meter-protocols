module Metering.Mbus.Devices.Tests.DeviceRuntimeTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders.Core
open Metering.Mbus.Devices
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Devices.Tests.DeviceTestSupport

// Reads and writes have independent positions, as on a serial duplex stream.
type private DuplexStream(input: byte[], chunkSize: int) =
    inherit Stream()
    let source = new MemoryStream(input)
    let output = new MemoryStream()
    member _.Output = output.ToArray()
    override _.CanRead = true
    override _.CanWrite = true
    override _.CanSeek = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Flush() = ()

    override _.Read(buffer, offset, count) =
        source.Read(buffer, offset, min count chunkSize)

    override _.Write(buffer, offset, count) = output.Write(buffer, offset, count)

    override _.ReadAsync(buffer: Memory<byte>, ct) =
        source.ReadAsync(buffer.Slice(0, min buffer.Length chunkSize), ct)

    override _.WriteAsync(buffer: ReadOnlyMemory<byte>, ct) = output.WriteAsync(buffer, ct)

    override _.Dispose(disposing) =
        if disposing then
            source.Dispose()
            output.Dispose()

        base.Dispose(disposing)

let private encode =
    Encoder.create
        EncodingOptions.Plain
        {
            new IExternalSecurityContextResolver with
                member _.ResolveMode5Context _ = None
        }

let private responseBytes =
    let data =
        Array.concat [ [| 0x08uy; 0x05uy; 0x72uy |]; address; [| 0uy; 0uy; 0uy; 0uy |] ]

    let checksum = byte ((data |> Array.sumBy int) &&& 255)
    Array.concat [ [| 0x68uy; 15uy; 15uy; 0x68uy |]; data; [| checksum; 0x16uy |] ]

[<Fact>]
let ``runtime selects requests and deselects across fragmented duplex reads`` () =
    task {
        let other = Array.copy address
        other[0] <- 0x79uy

        let input =
            Array.concat [
                selectionFrame 0x53uy 0xFDuy [||]
                shortFrame 0x5Buy 0xFDuy
                selectionFrame 0x53uy 0xFDuy other
                shortFrame 0x5Buy 0xFDuy
            ]

        for chunkSize in [ 1; 3; 261 ] do
            use stream = new DuplexStream(input, chunkSize)
            let mutable dataCalls = 0

            let getUserData () =
                dataCalls <- dataCalls + 1
                userData []

            do! DeviceRuntime.runAsync stream encode getUserData (initial ()) CancellationToken.None
            dataCalls |> should equal 1
            stream.Output |> should equal (Array.append [| 0xE5uy |] responseBytes)
    }

[<Fact>]
let ``runtime skips corrupt checksum and noise before a valid request`` () =
    task {
        let broken = selectionFrame 0x53uy 0xFDuy address
        broken[broken.Length - 2] <- 0uy

        use stream =
            new DuplexStream(Array.concat [ [| 0uy; 0x99uy |]; broken; shortFrame 0x5Buy 5uy ], 2)

        do!
            DeviceRuntime.runAsync
                stream
                encode
                (fun () -> userData [])
                (initial ())
                CancellationToken.None

        stream.Output |> should equal responseBytes
    }

[<Fact>]
let ``runtime preserves manufacturer data supplied with response records`` () =
    task {
        // EN 13757-3:2025, 6.6, pp. 38-39: DIF=0F precedes the final manufacturer block.
        let manufacturerBytes = [| 0xAAuy; 0xBBuy |]

        let data =
            RspUdData.create
                []
                (Some(MfrSpecificData.ofBytes (ReadOnlyMemory manufacturerBytes)))
                false

        use stream = new DuplexStream(shortFrame 0x5Buy 5uy, 1)

        do!
            DeviceRuntime.runAsync
                stream
                encode
                (fun () -> data)
                (initial ())
                CancellationToken.None

        let body =
            Array.concat [
                [| 0x08uy; 0x05uy; 0x72uy |]
                address
                [| 0uy; 0uy; 0uy; 0uy; 0x0Fuy |]
                manufacturerBytes
            ]

        let checksum = byte ((body |> Array.sumBy int) &&& 255)

        stream.Output
        |> should
            equal
            (Array.concat [
                [| 0x68uy; byte body.Length; byte body.Length; 0x68uy |]
                body
                [| checksum; 0x16uy |]
            ])
    }

[<Fact>]
let ``runtime rejects extended selection and still handles following requests`` () =
    task {
        let extended =
            Array.append address [| 0x0Cuy; 0x78uy; 0x78uy; 0x56uy; 0x34uy; 0x12uy |]

        use stream =
            new DuplexStream(
                Array.concat [ selectionFrame 0x53uy 0xFDuy extended; shortFrame 0x5Buy 0xFDuy ],
                261
            )

        do!
            DeviceRuntime.runAsync
                stream
                encode
                (fun () -> failwith "Unaddressed frame must not request user data")
                (initial ())
                CancellationToken.None

        stream.Output |> Array.isEmpty |> should be True
    }

[<Fact>]
let ``runtime reports truncated frame and clean EOF independently`` () =
    task {
        use empty = new DuplexStream([||], 1)

        do!
            DeviceRuntime.runAsync
                empty
                encode
                (fun () -> userData [])
                (initial ())
                CancellationToken.None

        use truncated = new DuplexStream([| 0x68uy; 0x0Buy; 0x0Buy; 0x68uy; 0x53uy |], 1)

        let! error =
            Record.ExceptionAsync(fun () ->
                DeviceRuntime.runAsync
                    truncated
                    encode
                    (fun () -> userData [])
                    (initial ())
                    CancellationToken.None)

        Assert.IsType<InvalidDataException>(error) |> ignore
    }

[<Fact>]
let ``runtime reports an oversized response without writing a partial frame`` () =
    task {
        use stream = new DuplexStream(shortFrame 0x5Buy 5uy, 5)

        let record: Metering.Mbus.Protocol.Records.DataRecordRsp = {
            Dib = {
                Fn = Metering.Mbus.Protocol.Records.DataInfoBlocks.FunctionField.Inst
                StNum = Metering.Mbus.Protocol.Records.DataInfoBlocks.StorageNumber.zero
                Tariff = Metering.Mbus.Protocol.Records.DataInfoBlocks.Tariff.zero
                SubUnit = Metering.Mbus.Protocol.Records.DataInfoBlocks.SubUnit.zero
            }
            Vib =
                Metering.Mbus.Protocol.Records.ValueInfoBlocks.VibRsp.Text(String.replicate 255 "u")
            Value = Metering.Mbus.Protocol.Records.Value.Text(String.replicate 191 "x")
        }

        let! error =
            Record.ExceptionAsync(fun () ->
                DeviceRuntime.runAsync
                    stream
                    encode
                    (fun () -> userData [ record; record ])
                    (initial ())
                    CancellationToken.None)

        Assert.IsType<InvalidDataException>(error) |> ignore
        stream.Output |> Array.isEmpty |> should be True
    }

[<Fact>]
let ``runtime cancellation interrupts a blocked read`` () =
    task {
        let entered =
            TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

        use stream = {
            new MemoryStream() with
                override _.ReadAsync(_: Memory<byte>, ct) =
                    ValueTask<int>(
                        task {
                            entered.SetResult()
                            do! Task.Delay(Timeout.Infinite, ct)
                            return 0
                        }
                    )
        }

        use cts = new CancellationTokenSource()

        let running =
            DeviceRuntime.runAsync stream encode (fun () -> userData []) (initial ()) cts.Token

        do! entered.Task
        cts.Cancel()
        let! error = Record.ExceptionAsync(fun () -> running)
        Assert.IsAssignableFrom<OperationCanceledException>(error) |> ignore
    }

[<Fact>]
let ``runtime uses the injected mode 5 encoder without storing its security context`` () =
    task {
        let resolver = {
            new IExternalSecurityContextResolver with
                member _.ResolveMode5Context meter =
                    meter |> should equal identity

                    Some(
                        Mode5ExternalContext(
                            Metering.Common.Security.Cryptography.Secret128.TakeOwnership(
                                Array.zeroCreate<byte> 16
                            )
                        )
                    )
        }

        let context = { SecondaryAddress = identity }

        let protectedEncode = Encoder.create (EncodingOptions.Mode5 context) resolver
        use stream = new DuplexStream(shortFrame 0x5Buy 5uy, 1)

        do!
            DeviceRuntime.runAsync
                stream
                protectedEncode
                (fun () -> userData [])
                (initial ())
                CancellationToken.None

        stream.Output[17..18] |> should equal [| 0xF0uy; 5uy |]

        match Metering.Mbus.Codec.Decoder.create resolver (ReadOnlyMemory stream.Output) with
        | Decoded(Message.SecondaryStationMessage(SecondaryStationMessage.ResponseUserData response),
                  _) ->
            response.UserData |> Seq.isEmpty |> should be True
            response.MeterAddress.IdNum |> should equal identity.IdNum
        | _ -> failwith "Expected a decrypted response"
    }

[<Fact>]
let ``runtime supplies device access number to the encoder for each response`` () =
    task {
        let access = Metering.Mbus.Protocol.Frames.TransportLayer.AccessNumber.create 42uy

        let state = {
            initial () with
                AccessNumber = access
        }

        let mutable calls = 0

        let encodeResponse (context: EncodingContext) message =
            context.AccessNumber |> should equal access
            calls <- calls + 1
            encode context message

        use stream = new DuplexStream(shortFrame 0x5Buy 5uy, 1)

        do!
            DeviceRuntime.runAsync
                stream
                encodeResponse
                (fun () -> userData [])
                state
                CancellationToken.None

        calls |> should equal 1
        stream.Output[15] |> should equal 42uy
    }
