namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.WiredMbus

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
            match field.Value.LinkUserData with
            | LinkUserData.MbusProtocol (MbusProtocolUserData.Complete message) ->
                Some (field.Value, message.Value)
            | _ ->
                None
        | _ ->
            None

    let private isResponseCi =
        function
        | Tpl.ShortHeader tpl ->
            tpl.Ci.Value = CiFieldTplShortHeader.Response
        | Tpl.LongHeader tpl ->
            tpl.Ci.Value = CiFieldTplLongHeader.Response
        | Tpl.NoneHeader _ ->
            false

    let private responseControl (frame: VariableLengthFrame) =
        match frame.CField.Value with
        | CField.Secondary secondary
            when secondary.Func.Value = SecondaryFunction.ResponseUserData ->
            Some secondary
        | _ ->
            None

    let private unprotectionFailed (protectedApl: AplProtectedRaw) =
        match protectedApl.Error with
        | UnprotectionError.Encryption error ->
            failed
                protectedApl.Bytes
                $"RSP_UD application payload could not be unprotected: {EncryptionError.value error}"
        | UnprotectionError.Validation failures ->
            Failed (failures, [])

    let matchesFrame frame =
        match completeMessage frame with
        | Some (dll, message) ->
            responseControl dll |> Option.isSome
            && isResponseCi message.Tpl.Value
        | None ->
            false

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<ResponseUserData> =

        validator {
            match completeMessage frame with
            | Some (dll, message) ->
                match responseControl dll with
                | None ->
                    return!
                        frameFailed
                            frame
                            "frame does not use the secondary RSP_UD control function."

                | Some control ->
                    if not (isResponseCi message.Tpl.Value) then
                        return!
                            frameFailed
                                frame
                                "frame does not use an RSP_UD response CI field."
                    else
                        match message.Apl with
                        | Apl.Protected protectedApl ->
                            return! unprotectionFailed protectedApl

                        | Apl.RspUdData response ->
                            match longHeader message.Tpl.Value with
                            | Some (device, accessNumber, status) ->
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

                            | None ->
                                return!
                                    frameFailed
                                        frame
                                        "RSP_UD legacy message mapping requires a long TPL header with meter identification."

                        | _ ->
                            return!
                                frameFailed
                                    frame
                                    "RSP_UD frame does not contain response user-data APL."

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
