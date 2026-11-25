namespace Metering.Mbus.Devices

open System
open System.IO
open System.Threading
open Metering.Common.Io
open Metering.Common.Decoding.Decoders.Core
open Metering.Mbus.Codec
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module DeviceRuntime =

    let private confirmation =
        Metering.Mbus.Protocol.Utility.Writer.runFt12Writer SingleCharacterFrame.write
        |> Result.defaultWith (fun error -> invalidOp error.Msg)
        |> ReadOnlyMemory

    let private negativeConfirmation =
        Metering.Mbus.Protocol.Utility.Writer.runFt12Writer SingleCharacterFrame.writeNegative
        |> Result.defaultWith (fun error -> invalidOp error.Msg)
        |> ReadOnlyMemory

    let private noSecurity = {
        new IExternalSecurityContextResolver with
            member _.ResolveMode5Context _ = None
    }

    let private decode = Decoder.create noSecurity

    let private parse (data: ReadOnlyMemory<byte>) =
        let complete length =
            if data.Length < length then
                StreamReader.Incomplete
            else
                // Fields retain slices of this owned copy, never the sliding buffer.
                let bytes = ReadOnlyMemory(data.Slice(0, length).ToArray())

                match decode bytes with
                | Decoded(message, _) -> StreamReader.Parsed(message, length)
                | DecodeFailed _ -> StreamReader.Invalid

        match data.Span[0] with
        | 0xE5uy -> complete 1
        | 0x10uy -> complete 5
        | 0x68uy ->
            if data.Length < 4 then
                StreamReader.Incomplete
            elif data.Span[1] <> data.Span[2] || data.Span[3] <> 0x68uy then
                StreamReader.Invalid
            else
                complete (int data.Span[1] + 6)
        | _ -> StreamReader.Invalid

    let runAsync
        (stream: Stream)
        (encodeResponse: EncodingContext -> Message -> Result<byte[], EncodeError>)
        (getUserData: unit -> RspUdData)
        (initialState: DeviceState)
        (ct: CancellationToken)
        =
        task {
            let read = StreamReader.create stream 261 parse
            let write = StreamWriter.create stream id
            let mutable state = initialState
            let mutable running = true

            while running do
                ct.ThrowIfCancellationRequested()

                match! read ct with
                | StreamReader.EndOfInput -> running <- false
                | StreamReader.TruncatedInput bytes ->
                    raise (
                        InvalidDataException(
                            $"Truncated frame at end of stream: %A{bytes.ToArray()}"
                        )
                    )
                | StreamReader.FrameRead message ->
                    let access = state.AccessNumber

                    let data =
                        if DeviceLogic.needsUserData state message then
                            getUserData ()
                        else
                            RspUdData.create [] None false

                    let newState, action = DeviceLogic.handleFrame state data message
                    state <- newState

                    let send message =
                        task {
                            match encodeResponse { AccessNumber = access } message with
                            | Ok bytes -> do! write (ReadOnlyMemory bytes) ct
                            | Error error ->
                                raise (
                                    InvalidDataException($"Cannot encode response: {error.Message}")
                                )
                        }

                    match action with
                    | SendConfirmation -> do! write confirmation ct
                    | SendNegativeConfirmation -> do! write negativeConfirmation ct
                    | SendUserData response ->
                        do!
                            send (
                                Message.SecondaryStationMessage(
                                    SecondaryStationMessage.ResponseUserData response
                                )
                            )
                    | SendSelectedApplication(index, response) ->
                        match ResponseSelectedApplication.ofUserData index response with
                        | Ok selected ->
                            do!
                                send (
                                    Message.SecondaryStationMessage(
                                        SecondaryStationMessage.ResponseSelectedApplication selected
                                    )
                                )
                        | Error error -> raise (InvalidDataException(error.Msg))
                    | NoAction -> ()
        }
