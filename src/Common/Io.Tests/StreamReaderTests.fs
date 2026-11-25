module Metering.Common.Io.Tests.StreamReaderTests

open System
open System.IO
open System.Threading
open Xunit
open FsUnit.Xunit
open Metering.Common.Io

// A deliberately non-M-Bus protocol verifies that Common owns no frame rules.
let private parse (bytes: ReadOnlyMemory<byte>) =
    if bytes.Span[0] <> 0xAAuy then StreamReader.Invalid
    elif bytes.Length < 3 then StreamReader.Incomplete
    else StreamReader.Parsed(bytes.Slice(1, 2).ToArray(), 3)

[<Fact>]
let ``reader skips noise returns owned frames and reaches clean EOF`` () =
    task {
        use stream = new MemoryStream([| 0uy; 0xAAuy; 1uy; 2uy; 0xAAuy; 3uy; 4uy |])
        let read = StreamReader.create stream 4 parse
        let! first = read CancellationToken.None
        let! second = read CancellationToken.None
        let! last = read CancellationToken.None
        first |> should equal (StreamReader.FrameRead [| 1uy; 2uy |])
        second |> should equal (StreamReader.FrameRead [| 3uy; 4uy |])

        last
        |> should equal (StreamReader.EndOfInput: StreamReader.ReadFrameResult<byte[]>)
    }

[<Fact>]
let ``reader reports a truncated final frame`` () =
    task {
        use stream = new MemoryStream([| 0xAAuy; 1uy |])
        let read = StreamReader.create stream 4 parse

        match! read CancellationToken.None with
        | StreamReader.TruncatedInput bytes -> bytes.ToArray() |> should equal [| 0xAAuy; 1uy |]
        | actual -> failwith $"Expected truncated input, got {actual}"
    }

[<Theory>]
[<InlineData(0)>]
[<InlineData(2)>]
let ``reader rejects invalid consumed lengths`` consumed =
    task {
        use stream = new MemoryStream([| 1uy |])
        let read = StreamReader.create stream 4 (fun _ -> StreamReader.Parsed((), consumed))

        let! error =
            Record.ExceptionAsync(fun () ->
                task { let! _ = read CancellationToken.None in return () })

        Assert.IsType<InvalidOperationException>(error) |> ignore
    }

[<Fact>]
let ``reader respects cancellation before consuming input`` () =
    task {
        use stream = new MemoryStream([| 0xAAuy; 1uy; 2uy |])
        use cts = new CancellationTokenSource()
        cts.Cancel()
        let read = StreamReader.create stream 4 parse

        let! error =
            Record.ExceptionAsync(fun () -> task { let! _ = read cts.Token in return () })

        Assert.IsAssignableFrom<OperationCanceledException>(error) |> ignore
        stream.Position |> should equal 0L
    }
