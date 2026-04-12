namespace Mbus.Io

open System
open System.IO
open System.Threading
open Mbus.Frames
open Mbus.BaseParsers.Core

module StreamReader =

    type ReadFrameResult =
        | FrameRead of Frame
        | EndOfInput
        | TruncatedInput of ReadOnlyMemory<byte>

    type private ParseResult =
        | Incomplete
        | Parsed of Frame * int
        | Invalid

    let private tryMatchFrame expectedStart minLength parser (data: ReadOnlyMemory<byte>) =
        if not data.IsEmpty && data.Span[0] = expectedStart then
            if data.Length < minLength then Some Incomplete
            else parser data
        else None

    let private (|ConfirmationFrame|_|) =
        tryMatchFrame Frame.confirmationStartByte 1 (fun _ -> Some (Parsed (Confirmation, 1)))

    let private (|ShortFrame|_|) =
        tryMatchFrame Frame.shortFrameStartByte 5 (fun data ->
            match FrameParser.parseShortFrame { Buf = data.Slice(0, 5); Off = 0 } with
            | Ok (sf, _) -> Some (Parsed (Frame.ShortFrame sf, 5))
            | Error _ -> Some Invalid)

    let private (|LongFrame|_|) =
        tryMatchFrame Frame.longFrameStartByte 4 (fun data ->
            match FrameParser.parseLongFrameHeader { Buf = data; Off = 0 } with
            | Ok ((len, _), _) ->
                let totalLen = 4 + len + 2
                if data.Length < totalLen then Some Incomplete
                else
                    match FrameParser.parseLongFrame { Buf = data.Slice(0, totalLen); Off = 0 } with
                    | Ok (lf, _) -> Some (Parsed (Frame.LongFrame lf, totalLen))
                    | Error _ -> Some Invalid
            | Error _ -> Some Invalid)

    let private tryParseFrame (data: ReadOnlyMemory<byte>) : ParseResult =
        if data.IsEmpty then Incomplete
        else
            match data with
            | ConfirmationFrame res
            | ShortFrame res
            | LongFrame res -> res
            | _ -> Invalid

    let create (stream: Stream) bufferSize =
        let window = SlidingWindow(stream, bufferSize)

        fun (ct: CancellationToken) -> task {
            let mutable result = EndOfInput
            let mutable found = false

            while not found do
                ct.ThrowIfCancellationRequested()
                match tryParseFrame window.Data with
                | Parsed (res, bytesConsumed) ->
                    window.Advance bytesConsumed
                    result <- FrameRead res
                    found <- true

                | Invalid ->
                    window.Advance 1

                | Incomplete ->
                    let! n = window.FillAsync ct
                    if n = 0 then
                        result <-
                            if window.Data.IsEmpty then EndOfInput
                            else TruncatedInput window.Data
                        found <- true

            return result
        }
