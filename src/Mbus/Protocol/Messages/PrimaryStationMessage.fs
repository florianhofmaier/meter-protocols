namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.WiredMbus

type PrimaryStationMessage =
    | SendUserData2 of SendUserData2
    | ApplicationResetOrSelect of ApplicationResetOrSelect
    | RequestSelectedApplication of RequestSelectedApplication
    | LinkLayerReset of LinkLayerReset
    | SelectionOfDevice of SelectionOfDevice
    | ChangePrimaryAddress of ChangePrimaryAddress
    | RequestUserData of RequestUserData
    | RequestAlarms of RequestAlarms

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrimaryStationMessage =

    let private frameFailed frame message =
        failed (WiredMbusFrame.asField frame) message

    let matchesFrame (frame: WiredMbusFrame) : bool =
        ApplicationResetOrSelect.matchesFrame frame
        || RequestSelectedApplication.matchesFrame frame
        || RequestAlarms.matchesFrame frame
        || RequestUserData.matchesFrame frame
        || ChangePrimaryAddress.matchesFrame frame
        || SelectionOfDevice.matchesFrame frame
        || LinkLayerReset.matchesFrame frame

    let fromFrame (frame: WiredMbusFrame) : Validation<PrimaryStationMessage> =

        if ApplicationResetOrSelect.matchesFrame frame then
            ApplicationResetOrSelect.fromFrame frame
            |> map PrimaryStationMessage.ApplicationResetOrSelect
        elif RequestSelectedApplication.matchesFrame frame then
            RequestSelectedApplication.fromFrame frame
            |> map PrimaryStationMessage.RequestSelectedApplication
        elif RequestAlarms.matchesFrame frame then
            RequestAlarms.fromFrame frame |> map PrimaryStationMessage.RequestAlarms
        elif RequestUserData.matchesFrame frame then
            frame |> RequestUserData.fromFrame |> map PrimaryStationMessage.RequestUserData
        elif ChangePrimaryAddress.matchesFrame frame then
            frame
            |> ChangePrimaryAddress.validate
            |> map PrimaryStationMessage.ChangePrimaryAddress
        elif SelectionOfDevice.matchesFrame frame then
            frame
            |> SelectionOfDevice.fromFrame
            |> map PrimaryStationMessage.SelectionOfDevice
        elif LinkLayerReset.matchesFrame frame then
            frame |> LinkLayerReset.fromFrame |> map PrimaryStationMessage.LinkLayerReset
        else
            frameFailed frame "frame is not a supported primary station message"

    let tryFromFrame (frame: WiredMbusFrame) : PrimaryStationMessage option =

        match fromFrame frame with
        | Passed(message, _) -> Some message

        | Failed _ -> None

    /// Writes the selected primary-station message in its plain representation.
    let write =
        function
        | PrimaryStationMessage.SendUserData2 message -> SendUserData2.write message
        | PrimaryStationMessage.ApplicationResetOrSelect message ->
            ApplicationResetOrSelect.write message
        | PrimaryStationMessage.RequestSelectedApplication message ->
            RequestSelectedApplication.write message
        | PrimaryStationMessage.LinkLayerReset message -> LinkLayerReset.write message
        | PrimaryStationMessage.SelectionOfDevice message -> SelectionOfDevice.write message
        | PrimaryStationMessage.ChangePrimaryAddress message -> ChangePrimaryAddress.write message
        | PrimaryStationMessage.RequestUserData message -> RequestUserData.write message
        | PrimaryStationMessage.RequestAlarms message -> RequestAlarms.write message
