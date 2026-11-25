namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

module Selection = Metering.Mbus.Protocol.Frames.ApplicationLayer.ApplicationResetOrSelect

type ApplicationResetOrSelect = {
    Address: AField
    Selection: Metering.Mbus.Protocol.Frames.ApplicationLayer.ApplicationResetOrSelect
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ApplicationResetOrSelect =
    let private command =
        function
        | WiredMbusFrame.VariableLength(dll,
                                        LinkUserData.MbusProtocol(MbusProtocolUserData.Complete message)) ->
            match dll.Value.CField.Value, message.Value.Apl.Value with
            | CField.Primary control, Apl.ApplicationResetOrSelect selection when
                control.Func.Value = PrimaryFunction.SendUserData && control.Fcv
                ->
                Some(dll.Value.AField.Value, selection)
            | _ -> None
        | _ -> None

    let matchesFrame frame = command frame |> Option.isSome

    let fromFrame frame =
        match command frame with
        | Some(address, selection) ->
            passed {
                Address = address
                Selection = selection
            }
        | None ->
            failed (WiredMbusFrame.asField frame) "Expected a SND-UD application reset or select"

    let write (message: ApplicationResetOrSelect) =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false true)
            message.Address
            (writer {
                do! writeU8 0x50uy

                do! Selection.write message.Selection
            })
