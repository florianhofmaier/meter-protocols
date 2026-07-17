namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module ResponseUserData =

    let tryFromFrame
        (_frame: Frame)
        : ResponseUserData option =

        None
