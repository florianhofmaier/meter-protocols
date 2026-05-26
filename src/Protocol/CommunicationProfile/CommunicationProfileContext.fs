module Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

type CommunicationProfileContext =
    // | Hdlc of Hdlc.HdlcContext
    | TcpUdpIp of TcpUdpIpContext
    // | Coap of Coap.CoapContext
    // | SFskPlc of SFskPlc.SFskPlcContext
    // | MBus of MBus.MBusContext
    // | SmsShortWrapper of Sms.SmsShortWrapperContext
    // | Lpwan of Lpwan.LpwanContext
    // | WiSun of WiSun.WiSunContext
    // | Gateway of Gateway.GatewayContext