namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResponseUserData =

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
        (_frame: WiredMbusFrame)
        : bool =

        false

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<ResponseUserData> =

        frameFailed frame "frame is not a supported RSP_UD response user data message"

    let tryFromFrame
        (frame: WiredMbusFrame)
        : ResponseUserData option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
