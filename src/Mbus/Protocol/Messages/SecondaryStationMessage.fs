namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SecondaryStationMessage =

    let private frameFailed frame message =
        let fieldId =
            match frame with
            | Frame.SingleCharacter field ->
                field.Id
            | Frame.FixedLength field ->
                field.Id
            | Frame.VariableLength field ->
                field.Id

        Failed (
            Failures.single {
                FieldId = fieldId
                Message = message
            },
            []
        )

    let matchesFrame
        (frame: Frame)
        : bool =

        ResponseUserData.matchesFrame frame

    let fromFrame
        (frame: Frame)
        : Validation<SecondaryStationMessage> =

        if ResponseUserData.matchesFrame frame then
            frame
            |> ResponseUserData.fromFrame
            |> map SecondaryStationMessage.ResponseUserData
        else
            frameFailed frame "frame is not a supported secondary station message"

    let tryFromFrame
        (frame: Frame)
        : SecondaryStationMessage option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
