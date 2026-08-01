namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.UserData
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResponseUserData =

    let private frameFailed frame message =
        let fieldId =
            match frame with
            | WiredMbusFrame.SingleCharacter field -> field.Id
            | WiredMbusFrame.FixedLength field -> field.Id
            | WiredMbusFrame.VariableLength field -> field.Id

        Failed (
            Failures.single {
                FieldId = fieldId
                Message = message
            },
            []
        )

    let private longHeader =
        function
        | Tpl.LongHeader tpl ->
            match tpl.Header.Value with
            | LongHeader.Mode0 header ->
                Some (header.Device, header.Acc.Value, header.Status.Value)
            | LongHeader.Mode5 header ->
                Some (header.Device, header.Acc.Value, header.Status.Value)

        | _ ->
            None

    let private completeMessage =
        function
        | WiredMbusFrame.VariableLength field ->
            match field.Value with
            | FrameVariableLength.CompleteMessage message -> Some message
        | _ ->
            None

    let private responseControl message =
        match message.Dll.Value.CField.Value with
        | CField.Secondary secondary
            when secondary.Func.Value = SecondaryFunction.ResponseUserData ->
            Some secondary
        | _ ->
            None

    let private decodedResponse message =
        match message.Payload with
        | AplContent.Decoded apl ->
            match apl.Value with
            | Apl.RspUdData response -> Some response
            | _ -> None
        | AplContent.Protected _ ->
            None

    let matchesFrame frame =
        match completeMessage frame with
        | Some message ->
            responseControl message |> Option.isSome
            && decodedResponse message |> Option.isSome
            && longHeader message.Tpl.Value |> Option.isSome
        | None ->
            false

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<ResponseUserData> =

        validator {
            match completeMessage frame with
            | Some message ->
                match responseControl message, message.Payload with
                | Some _, AplContent.Protected _ ->
                    return!
                        frameFailed
                            frame
                            "RSP_UD application payload is protected and unavailable; decoded response data is required."

                | Some control, AplContent.Decoded apl ->
                    match apl.Value, longHeader message.Tpl.Value with
                    | Apl.RspUdData response,
                      Some (device, accessNumber, status) ->
                        return {
                            Acd = control.Acd
                            Dfc = control.Dfc
                            Status = status
                            AccessNumber = accessNumber
                            MeterAddress = {
                                IdNum = device.IdNum.Value
                                Mfr = device.Mfr.Value
                                Version = device.Version.Value
                                DevType = device.DevType.Value
                            }
                            UserData = RspUdData.records response
                            MfrData = RspUdData.mfrData response
                            MoreFollows = RspUdData.moreFollows response
                        }

                    | Apl.RspUdData _, None ->
                        return!
                            frameFailed
                                frame
                                "RSP_UD legacy message mapping requires a long TPL header with meter identification."

                    | _ ->
                        return!
                            frameFailed
                                frame
                                "RSP_UD frame does not contain response user-data APL."

                | None, _ ->
                    return!
                        frameFailed
                            frame
                            "frame does not use the secondary RSP_UD control function."

            | None ->
                return!
                    frameFailed
                        frame
                        "frame is not a complete variable-length RSP_UD message"
        }

    let tryFromFrame
        (frame: WiredMbusFrame)
        : ResponseUserData option =

        match fromFrame frame with
        | Passed (message, _) -> Some message
        | Failed _ -> None
