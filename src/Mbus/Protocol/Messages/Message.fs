namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

    let private frameFailed frame message =
        let fieldId =
            match frame with
            | WiredMbusFrame.SingleCharacter field ->
                field.Id
            | WiredMbusFrame.FixedLength field ->
                field.Id
            | WiredMbusFrame.VariableLength field ->
                field.Id

        Failed (
            Failures.single {
                FieldId = fieldId
                Message = message
            },
            []
        )

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<Message> =

        if PrimaryStationMessage.matchesFrame frame then
            frame
            |> PrimaryStationMessage.fromFrame
            |> map Message.PrimaryStationMessage
        elif SecondaryStationMessage.matchesFrame frame then
            frame
            |> SecondaryStationMessage.fromFrame
            |> map Message.SecondaryStationMessage
        else
            frameFailed frame "frame is not a supported M-Bus message"

    let tryFromFrame
        (frame: WiredMbusFrame)
        : Message option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
