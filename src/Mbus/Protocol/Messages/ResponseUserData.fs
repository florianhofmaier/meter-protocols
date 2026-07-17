namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResponseUserData =

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
        (_frame: Frame)
        : bool =

        false

    let fromFrame
        (frame: Frame)
        : Validation<ResponseUserData> =

        frameFailed frame "frame is not a supported RSP_UD response user data message"

    let tryFromFrame
        (frame: Frame)
        : ResponseUserData option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
