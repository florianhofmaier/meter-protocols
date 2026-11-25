namespace Metering.Common.Io

open System
open System.IO
open System.Threading

/// <summary>Serializes concurrent stream writes through a shared semaphore.</summary>
module StreamWriter =

    /// <summary>Creates a cancellable writer. Encoding, writing and flushing occur under the same lock.</summary>
    let create (stream: Stream) (encode: 'frame -> ReadOnlyMemory<byte>) =
        let gate = new SemaphoreSlim(1, 1)

        fun (frame: 'frame) (ct: CancellationToken) ->
            task {
                do! gate.WaitAsync ct

                try
                    let bytes = encode frame
                    do! stream.WriteAsync(bytes, ct)
                    do! stream.FlushAsync ct
                finally
                    gate.Release() |> ignore
            }
