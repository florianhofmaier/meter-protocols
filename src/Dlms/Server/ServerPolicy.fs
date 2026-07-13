namespace Metering.Dlms.Server

open Metering.Dlms.Protocol.ApplicationLayer

type ServerPolicy =
    {
        DecideOpen : CosemOpenIndication -> CosemOpenResponse
    }