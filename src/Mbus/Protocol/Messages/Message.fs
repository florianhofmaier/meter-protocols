namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames.WiredMbus

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

    let private frameFailed frame message =
        failed (WiredMbusFrame.asField frame) message

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<Message> =

        if PrimaryStationMessage.matchesFrame frame then
            frame
            |> PrimaryStationMessage.fromFrame
            |> map Message.PrimaryStationMessage
        elif SecondaryStationMessage.matchesFrame frame then
            frame
            |> SecondaryStationMessage.fromFrame
            |> map Message.SecondaryStationMessage
        else
            frameFailed frame "frame is not a supported M-Bus message"

    let tryFromFrame
        (frame: WiredMbusFrame)
        : Message option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None

    let fromRaw (frame: Field<WiredMbusFrame>) : Validation<Field<Message>> =
        validateField (Field.value >> fromFrame) frame

    /// Final semantic codec: consumes the assembled frame, without calling other codecs.
    let decode (frame: Field<WiredMbusFrame>) : Decoder<Field<Message>> =
        validate fromRaw frame
