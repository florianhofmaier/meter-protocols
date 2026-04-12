module Mbus.Io.Tests.SlidingWindowTests

open Xunit
open FsUnit.Xunit
open Shouldly
open System.IO
open System.Threading
open System.Threading.Tasks
open Mbus.Io

[<Fact>]
let ``Constructor WhenCalled ShouldInitializeEmptyWindow`` () =
    use ms = new MemoryStream()
    let window = SlidingWindow(ms, 10)
    window.Data.Length |> should equal 0

[<Fact>]
let ``FillAsync WhenStreamHasData ShouldFillBuffer`` () =
    let data = [| 1uy; 2uy; 3uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 10)

    window.FillAsync(CancellationToken.None).Wait()

    window.Data.Length |> should equal 3
    window.Data.ToArray() |> should equal data

[<Fact>]
let ``Advance WhenCalled ShouldUpdateOffsetAndCount`` () =
    let data = [| 1uy; 2uy; 3uy; 4uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 10)

    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(1)

    window.Data.Length.ShouldBe(3)
    window.Data.ToArray().ShouldBe([| 2uy; 3uy; 4uy |])

[<Fact>]
let ``FillAsync WhenStreamIsEmpty ShouldReturnZero`` () =
    task {
        use ms = new MemoryStream()
        let window = SlidingWindow(ms, 10)
        let! n = window.FillAsync(CancellationToken.None)
        n.ShouldBe(0)
    }

[<Fact>]
let ``FillAsync WhenBufferIsFull ShouldThrowMbusIoException`` () =
    task {
        let largerData = Array.init 11 byte
        use msLarge = new MemoryStream(largerData)
        let window = SlidingWindow(msLarge, 10)

        let! n = window.FillAsync(CancellationToken.None)
        n.ShouldBe(10)
        let! _ = Assert.ThrowsAsync<MbusIoException>(fun () -> window.FillAsync(CancellationToken.None))
        ()
    }

[<Fact>]
let ``FillAsync WhenOffsetAtEnd ShouldCompactAndFillResultingInBuffer`` () =
    let allData = [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy |]
    use ms = new MemoryStream(allData)
    let window = SlidingWindow(ms, 5)
    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(3)

    window.FillAsync(CancellationToken.None).Wait()

    window.Data.ToArray().ShouldBe([| 4uy; 5uy; 6uy; 7uy |])
    window.Data.Length.ShouldBe(4)

[<Fact>]
let ``Advance WhenByTotalCount ShouldMakeWindowEmpty`` () =
    let data = [| 1uy; 2uy; 3uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 10)
    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(3)
    window.Data.Length.ShouldBe(0)

[<Fact>]
let ``FillAsync AfterCompleteAdvance ShouldWorkCorrectly`` () =
    let data = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 5)
    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(5)

    let n = window.FillAsync(CancellationToken.None).Result

    n.ShouldBe(0)

[<Fact>]
let ``FillAsync WhenCancelled ShouldThrowTaskCanceledException`` () =
    let ms = new MemoryStream([| 1uy |])
    let window = SlidingWindow(ms, 10)
    let cts = new CancellationTokenSource()
    cts.Cancel()

    Assert.ThrowsAsync<TaskCanceledException>(fun () -> window.FillAsync(cts.Token))


[<Fact>]
let ``Advance WhenZero ShouldDoNothing`` () =
    let data = [| 1uy; 2uy; 3uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 10)
    window.FillAsync(CancellationToken.None).Wait()

    window.Advance(0)

    window.Data.Length.ShouldBe(3)
    window.Data.ToArray().ShouldBe(data)

[<Fact>]
let ``FillAsync AfterMultipleAdvances ShouldCompactAndFillCorrectly`` () =
    let data = [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy; 11uy; 12uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 5)
    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(2)
    window.FillAsync(CancellationToken.None).Wait()
    window.Data.ToArray().ShouldBe([| 3uy; 4uy; 5uy; 6uy; 7uy |])
    window.Advance(4)

    window.FillAsync(CancellationToken.None).Wait()

    window.Data.ToArray().ShouldBe([| 7uy; 8uy; 9uy; 10uy; 11uy |])

[<Fact>]
let ``FillAsync WhenBufferExactFullAfterCompact ShouldThrow`` () =
    let data = [| 1uy; 2uy; 3uy; 4uy; 5uy |]
    use ms = new MemoryStream(data)
    let window = SlidingWindow(ms, 3)
    window.FillAsync(CancellationToken.None).Wait()
    window.Advance(2)
    window.FillAsync(CancellationToken.None).Wait()
    window.Data.ToArray().ShouldBe([| 3uy; 4uy; 5uy |])

    Assert.ThrowsAsync<MbusIoException>(fun () -> window.FillAsync(CancellationToken.None))

