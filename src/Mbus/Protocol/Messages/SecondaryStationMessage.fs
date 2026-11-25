namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.WiredMbus

type SecondaryStationMessage =
    | ResponseSelectedApplication of ResponseSelectedApplication
    | ResponseUserData of ResponseUserData
    | ResponseAlarms of ResponseAlarms

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SecondaryStationMessage =

    let private frameFailed frame message =
        failed (WiredMbusFrame.asField frame) message

    let matchesFrame (frame: WiredMbusFrame) : bool =

        ResponseSelectedApplication.matchesFrame frame
        || ResponseUserData.matchesFrame frame

    let fromFrame (frame: WiredMbusFrame) : Validation<SecondaryStationMessage> =

        if ResponseSelectedApplication.matchesFrame frame then
            ResponseSelectedApplication.fromFrame frame
            |> map SecondaryStationMessage.ResponseSelectedApplication
        elif ResponseUserData.matchesFrame frame then
            frame
            |> ResponseUserData.fromFrame
            |> map SecondaryStationMessage.ResponseUserData
        else
            frameFailed frame "frame is not a supported secondary station message"

    let tryFromFrame (frame: WiredMbusFrame) : SecondaryStationMessage option =

        match fromFrame frame with
        | Passed(message, _) -> Some message

        | Failed _ -> None

    /// Writes the selected secondary-station message.
    let write header payload =
        function
        | SecondaryStationMessage.ResponseSelectedApplication message ->
            Metering.Common.BaseWriters.Core.writerError
                "Selected application responses require Encoder with an access-number context"
        | SecondaryStationMessage.ResponseUserData message ->
            ResponseUserData.write header payload message
        | SecondaryStationMessage.ResponseAlarms message ->
            ResponseAlarms.write header payload message
