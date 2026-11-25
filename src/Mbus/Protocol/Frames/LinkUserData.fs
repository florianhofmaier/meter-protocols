namespace Metering.Mbus.Protocol.Frames

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

type LinkUserData =
    | MbusProtocol of MbusProtocolUserData
    | SelectionOfDevice of DeviceSelection
