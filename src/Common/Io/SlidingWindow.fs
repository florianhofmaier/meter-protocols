namespace Metering.Common.Io

open System
open System.IO

exception MbusIoException of string

type SlidingWindow(stream: Stream, bufferSize: int) =
    let buffer = Array.zeroCreate<byte> bufferSize
    let mutable offset = 0
    let mutable count = 0

    let compact () =
        if count > 0 then
            Buffer.BlockCopy(buffer, offset, buffer, 0, count)

        offset <- 0

    /// <summary>Returns a view of the currently buffered unread bytes; later buffer operations can change the underlying contents.</summary>
    member this.Data = ReadOnlyMemory(buffer, offset, count)

    /// <summary>Discards the requested number of buffered bytes by advancing the window; the caller must supply a count within the buffered range.</summary>
    member this.Advance(n: int) =
        offset <- offset + n
        count <- count - n

    /// <summary>Reads more bytes into the window, compacting when needed; returns the number read, propagates cancellation and throws MbusIoException when the buffer is full.</summary>
    member this.FillAsync ct =
        task {
            if offset + count >= bufferSize then
                compact ()

            if count >= bufferSize then
                raise (MbusIoException "Frame too large or buffer overflow")

            let maxBytes = bufferSize - (offset + count)
            let target = Memory(buffer, offset + count, maxBytes)
            let! n = stream.ReadAsync(target, ct)
            count <- count + n
            return n
        }
