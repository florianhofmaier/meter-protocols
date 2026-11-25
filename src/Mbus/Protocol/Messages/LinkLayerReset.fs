namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.WiredMbus

type LinkLayerReset = { Address: AField }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkLayerReset =

    let private resetFrame =
        function
        | WiredMbusFrame.FixedLength frame ->
            match frame.Value.CField.Value with
            | CField.Primary control when control.Func.Value = PrimaryFunction.LinkReset ->
                Some(frame.Value, control)
            | _ -> None
        | _ -> None

    let matchesFrame frame = resetFrame frame |> Option.isSome

    let fromFrame frame : Validation<LinkLayerReset> =
        validator {
            match resetFrame frame with
            | Some(dll, control) ->
                let! () =
                    ensure
                        dll.CField
                        "SND_NKE requires FCB and FCV to be clear"
                        (not control.Fcb && not control.Fcv)

                return { Address = dll.AField.Value }
            | None ->
                return! failed (WiredMbusFrame.asField frame) "frame is not a SND_NKE link reset"
        }

    /// Writes SND-NKE, resetting the link's FCB/selection state.
    let write (message: LinkLayerReset) =
        FixedLengthFrame.write
            (CField.writePrimary PrimaryFunction.LinkReset false false)
            message.Address
