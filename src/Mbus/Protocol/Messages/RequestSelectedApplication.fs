namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Common.BaseWriters.BinaryWriters

type RequestSelectedApplication = { Address: AField; Immediate: bool }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RequestSelectedApplication =
    let private command =
        function
        | WiredMbusFrame.VariableLength(dll,
                                        LinkUserData.MbusProtocol(MbusProtocolUserData.Complete message)) ->
            match dll.Value.CField.Value, message.Value.Apl.Value with
            | CField.Primary control, Apl.RequestSelectedApplication when
                control.Func.Value = PrimaryFunction.SendUserData
                ->
                Some(dll.Value.AField.Value, not control.Fcv)
            | _ -> None
        | _ -> None

    let matchesFrame frame = command frame |> Option.isSome

    let fromFrame frame =
        match command frame with
        | Some(address, immediate) ->
            passed {
                Address = address
                Immediate = immediate
            }
        | None -> failed (WiredMbusFrame.asField frame) "Expected a request of selected application"

    let write (message: RequestSelectedApplication) =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false (not message.Immediate))
            message.Address
            (writeU8 0x54uy)
