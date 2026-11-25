namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.WiredMbus

type SelectionOfDevice = { Selection: DeviceSelection }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SelectionOfDevice =

    let private selectionFrame =
        function
        | WiredMbusFrame.VariableLength(dll, LinkUserData.SelectionOfDevice selection) ->
            match dll.Value.CField.Value, dll.Value.AField.Value with
            | CField.Primary control, AField.SelectionOfDevice when
                control.Func.Value = PrimaryFunction.SendUserData
                ->
                Some(dll.Value, control, selection)
            | _ -> None
        | _ -> None

    let matchesFrame frame = selectionFrame frame |> Option.isSome

    let fromFrame frame : Validation<SelectionOfDevice> =
        validator {
            match selectionFrame frame with
            | Some(dll, control, selection) ->
                let! () = ensure dll.CField "SELECT requires FCV to be set" control.Fcv
                return { Selection = selection }
            | None -> return! failed (WiredMbusFrame.asField frame) "frame is not a SELECT message"
        }

    /// Writes SELECT as SND-UD to address FD with CI=52.
    let write (message: SelectionOfDevice) =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false true)
            AField.SelectionOfDevice
            (DeviceSelection.write message.Selection)
