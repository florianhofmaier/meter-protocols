namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module SecondaryStationMessage =

    let tryFromFrame
        (frame: Frame)
        : SecondaryStationMessage option =

        ResponseUserData.tryFromFrame frame
        |> Option.map SecondaryStationMessage.ResponseUserData
