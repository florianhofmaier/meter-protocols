module Metering.Common.Io.Tests.StreamWriterTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Metering.Common.Io

[<Fact>]
let ``writer serializes shared-buffer encoding together with concurrent writes`` () =
    task {
        use output = new MemoryStream()

        use stream = {
            new MemoryStream() with
                override _.WriteAsync(bytes: ReadOnlyMemory<byte>, ct) =
                    ValueTask(
                        task {
                            do! Task.Yield()
                            do! output.WriteAsync(bytes, ct)
                        }
                    )
        }

        let buffer = Array.zeroCreate<byte> 2

        let write =
            StreamWriter.create stream (fun value ->
                Array.fill buffer 0 2 value
                ReadOnlyMemory buffer)

        let! _ =
            [| for value in 1uy .. 50uy -> write value CancellationToken.None |]
            |> Task.WhenAll

        let actual = output.ToArray()
        actual.Length |> should equal 100

        actual
        |> Array.chunkBySize 2
        |> Array.iter (fun pair -> pair[0] |> should equal pair[1])

        actual
        |> Array.chunkBySize 2
        |> Array.map Array.head
        |> Array.sort
        |> should equal [| 1uy .. 50uy |]
    }

[<Fact>]
let ``encoder failure releases the lock for a subsequent write`` () =
    task {
        use stream = new MemoryStream()

        let write =
            StreamWriter.create stream (fun value ->
                if value = 0uy then
                    invalidOp "encoding failed"

                ReadOnlyMemory [| value |])

        let! error = Record.ExceptionAsync(fun () -> write 0uy CancellationToken.None)
        Assert.IsType<InvalidOperationException>(error) |> ignore
        do! write 1uy CancellationToken.None
        stream.ToArray() |> should equal [| 1uy |]
    }

[<Fact>]
let ``canceled writer does not call encoder`` () =
    task {
        use stream = new MemoryStream()
        use cts = new CancellationTokenSource()
        cts.Cancel()

        let write =
            StreamWriter.create stream (fun () -> failwith "Must not encode canceled writes")

        let! error = Record.ExceptionAsync(fun () -> write () cts.Token)
        Assert.IsAssignableFrom<OperationCanceledException>(error) |> ignore
        stream.Length |> should equal 0L
    }
