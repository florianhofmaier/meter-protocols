namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module Message =

    let tryFromFrame
        (frame: Frame)
        : Message option =

        match PrimaryStationMessage.tryFromFrame frame with
        | Some message ->
            Some (Message.PrimaryStationMessage message)

        | None ->
            SecondaryStationMessage.tryFromFrame frame
            |> Option.map Message.SecondaryStationMessage
