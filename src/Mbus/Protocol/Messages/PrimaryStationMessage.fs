namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module PrimaryStationMessage =

    let tryFromFrame
        (frame: Frame)
        : PrimaryStationMessage option =

        RequestUserData.tryFromFrame frame
        |> Option.map PrimaryStationMessage.RequestUserData
