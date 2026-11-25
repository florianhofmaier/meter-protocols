namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.WiredMbus

type RequestUserData = { Fcb: bool; Address: AField }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RequestUserData =

    let private frameFailed frame message =
        failed (WiredMbusFrame.asField frame) message

    let private requestFrame =
        function
        | WiredMbusFrame.FixedLength frame ->
            let dll = frame.Value

            match dll.CField.Value with
            | CField.Primary control when control.Func.Value = PrimaryFunction.RequestUserDataClass2 ->
                Some(dll, control)
            | _ -> None
        | _ -> None

    let matchesFrame frame = requestFrame frame |> Option.isSome

    let fromFrame (frame: WiredMbusFrame) : Validation<RequestUserData> =
        validator {
            match requestFrame frame with
            | Some(dll, control) ->
                let! () = ensure dll.CField "REQ_UD2 requires FCV to be set" control.Fcv

                return {
                    Fcb = control.Fcb
                    Address = dll.AField.Value
                }
            | None -> return! frameFailed frame "frame is not a REQ_UD2 request user data message"
        }

    let tryFromFrame (frame: WiredMbusFrame) : RequestUserData option =

        match fromFrame frame with
        | Passed(message, _) -> Some message

        | Failed _ -> None

    /// Writes REQ-UD2 with FCV set.
    let write (message: RequestUserData) =
        FixedLengthFrame.write
            (CField.writePrimary PrimaryFunction.RequestUserDataClass2 message.Fcb true)
            message.Address
