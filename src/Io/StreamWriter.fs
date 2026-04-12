namespace Mbus.Io

open System
open System.IO
open System.Threading
open Mbus.Frames
open Mbus.BaseWriters.Core

module StreamWriter =

    let private serialize frame writeBuffer =
            let wState = { WState.Buf = writeBuffer; Pos = 0 }
            match FrameWriter.writeFrame frame wState with
            | Ok (_, endState) -> endState.Pos
            | Error e -> failwithf "Serialization failed: %s" e.Msg

    let create (stream: Stream) =
        let lock = new SemaphoreSlim(1, 1)
        let writeBuffer = Array.zeroCreate<byte> 512

        fun (frame: Frame) (ct: CancellationToken) -> task {
            let len = serialize frame writeBuffer

            do! lock.WaitAsync(ct)
            try
                do! stream.WriteAsync(ReadOnlyMemory(writeBuffer, 0, len), ct)
                do! stream.FlushAsync(ct)
            finally
                lock.Release() |> ignore
        }
