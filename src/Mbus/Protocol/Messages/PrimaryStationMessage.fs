namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrimaryStationMessage =

    let private frameFailed frame message =
        failed (WiredMbusFrame.asField frame) message

    let matchesFrame (frame: WiredMbusFrame) : bool =
        RequestUserData.matchesFrame frame

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<PrimaryStationMessage> =

        if RequestUserData.matchesFrame frame then
            frame
            |> RequestUserData.fromFrame
            |> map PrimaryStationMessage.RequestUserData
        else
            frameFailed frame "frame is not a supported primary station message"

    let tryFromFrame
        (frame: WiredMbusFrame)
        : PrimaryStationMessage option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
