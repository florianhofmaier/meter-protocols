namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrimaryStationMessage =

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

    let matchesFrame
        (frame: WiredMbusFrame)
        : bool =

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
