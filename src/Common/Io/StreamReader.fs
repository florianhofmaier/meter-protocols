namespace Metering.Common.Io

open System
open System.IO
open System.Threading

/// <summary>Buffered stream reading with protocol-specific recognition supplied by the caller.</summary>
module StreamReader =

    /// <summary>Result of inspecting the current buffered input without retaining its memory.</summary>
    type ParseResult<'frame> =
        | Incomplete
        | Parsed of frame: 'frame * bytesConsumed: int
        | Invalid

    /// <summary>A complete frame, clean end of input, or a copy of the truncated final input.</summary>
    type ReadFrameResult<'frame> =
        | FrameRead of 'frame
        | EndOfInput
        | TruncatedInput of ReadOnlyMemory<byte>

    /// <summary>Creates a cancellable reader that skips invalid bytes. Parsed values must own any retained input bytes.</summary>
    let create (stream: Stream) bufferSize (parse: ReadOnlyMemory<byte> -> ParseResult<'frame>) =
        let window = SlidingWindow(stream, bufferSize)

        fun (ct: CancellationToken) ->
            task {
                let mutable result = EndOfInput
                let mutable found = false

                while not found do
                    ct.ThrowIfCancellationRequested()

                    let parsed =
                        if window.Data.IsEmpty then
                            Incomplete
                        else
                            parse window.Data

                    match parsed with
                    | Parsed(frame, bytesConsumed) ->
                        if bytesConsumed <= 0 || bytesConsumed > window.Data.Length then
                            invalidOp "The frame parser returned an invalid consumed length."

                        window.Advance bytesConsumed
                        result <- FrameRead frame
                        found <- true
                    | Invalid -> window.Advance 1
                    | Incomplete ->
                        let! n = window.FillAsync ct

                        if n = 0 then
                            result <-
                                if window.Data.IsEmpty then
                                    EndOfInput
                                else
                                    TruncatedInput(ReadOnlyMemory(window.Data.ToArray()))

                            found <- true

                return result
            }
