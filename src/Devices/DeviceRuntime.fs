namespace Mbus.Devices

open System.IO
open System.Threading
open Mbus.Io
open Mbus.Records

module DeviceRuntime =

    let runAsync (stream: Stream) (getUserData: unit -> RspDataRecord list) (initialState: DeviceState) (ct: CancellationToken) =
        task {
            let frameReader = StreamReader.create stream 1024
            let writer = StreamWriter.create stream

            let mutable state = initialState
            let mutable running = true

            while running do
                ct.ThrowIfCancellationRequested()
                match! frameReader ct with
                | StreamReader.EndOfInput ->
                    running <- false
                | StreamReader.TruncatedInput bytes ->
                    raise (InvalidDataException($"Truncated frame at end of stream: %A{bytes.ToArray()}"))
                | StreamReader.FrameRead frame ->
                    let userData = getUserData()
                    let newState, action = DeviceLogic.handleFrame state userData frame

                    state <- newState

                    // IO: Execute side effects
                    match action with
                    | SendFrame response ->
                        do! writer response ct
                    | NoAction ->
                        ()
        }

