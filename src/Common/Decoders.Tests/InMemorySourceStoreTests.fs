module Metering.Common.Decoding.Decoders.Tests.InMemorySourceStoreTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Tests.TestSupport
open Metering.Common.Decoding.Parsers.Types

let private read count (reader: IByteReader) =
    match reader.Read count with
    | Ok bytes -> bytes
    | Error error -> failwith error.Msg

[<Fact>]
let ``source ids are deterministic unique and do not collide`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let first =
        store.AddRoot "root1" (memory [| 0x01uy |]) false

    let second =
        store.AddRoot "root2" (memory [| 0x02uy |]) false

    let derived =
        store.AddDerived "derived" { Source = first.Id; Offset = 0; Length = 1 } (SourceTransform.Decrypt "AES") (memory [| 0x03uy |]) true

    first.Id |> should equal (SourceId.create 0)
    second.Id |> should equal (SourceId.create 1)
    derived.Id |> should equal (SourceId.create 2)

[<Fact>]
let ``AddRoot stores metadata and bytes`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let source =
        store.AddRoot "root" (memory [| 0xAAuy; 0xBBuy |]) true

    source
    |> should equal {
        Id = SourceId.create 0
        Name = "root"
        Origin = None
        Transform = SourceTransform.Root
        Length = 2
        Sensitive = true
    }

    let reader =
        store.CreateReader source.Id

    read 2 reader |> fun bytes -> bytes.ToArray()
    |> should equal [| 0xAAuy; 0xBBuy |]

    readerFactory.Calls |> should equal [ { Bytes = [| 0xAAuy; 0xBBuy |]; Offset = 0 } ]

[<Fact>]
let ``AddDerived stores origin transform metadata and bytes`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let origin =
        {
            Source = SourceId.create 10
            Offset = 5
            Length = 3
        }

    let source =
        store.AddDerived "derived" origin (SourceTransform.Decompress "LZ") (memory [| 0xCCuy |]) false

    source
    |> should equal {
        Id = SourceId.create 0
        Name = "derived"
        Origin = Some origin
        Transform = SourceTransform.Decompress "LZ"
        Length = 1
        Sensitive = false
    }

    store.CreateReader source.Id
    |> read 1
    |> fun bytes -> bytes.ToArray()
    |> should equal [| 0xCCuy |]

[<Fact>]
let ``CreateReader returns independent readers at position zero`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let source =
        store.AddRoot "root" (memory [| 0xAAuy; 0xBBuy |]) false

    let first =
        store.CreateReader source.Id

    let second =
        store.CreateReader source.Id

    read 1 first |> fun bytes -> bytes.ToArray() |> should equal [| 0xAAuy |]
    first.Position |> should equal 1
    second.Position |> should equal 0
    read 2 second |> fun bytes -> bytes.ToArray() |> should equal [| 0xAAuy; 0xBBuy |]

    readerFactory.Calls
    |> should equal [
        { Bytes = [| 0xAAuy; 0xBBuy |]; Offset = 0 }
        { Bytes = [| 0xAAuy; 0xBBuy |]; Offset = 0 }
    ]

[<Fact>]
let ``zero length sources work`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let source =
        store.AddRoot "empty" (memory [||]) false

    source.Length |> should equal 0

    let reader =
        store.CreateReader source.Id

    reader.Remaining |> should equal 0
    read 0 reader |> fun bytes -> bytes.ToArray() |> should equal [||]

[<Fact>]
let ``unknown source id throws deliberate programming error`` () =
    let readerFactory =
        RecordingReaderFactory()

    let store =
        InMemorySourceStore(readerFactory.Create) :> ISourceStore

    let ex =
        Assert.Throws<InvalidOperationException>(fun () ->
            store.CreateReader(SourceId.create 99) |> ignore)

    ex.Message |> should equal "Unknown decode source id 99"
