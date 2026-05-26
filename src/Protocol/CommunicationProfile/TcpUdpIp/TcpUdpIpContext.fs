namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

type TcpPort =
    private
        TcpPort of uint16

type UdpPort =
    private
        UdpPort of uint16

type IpAddress =
    private
        IpAddress of byte array

type TcpWrapperContext =
    {
        ServerIpAddress : IpAddress
        ServerTcpPort : TcpPort
        ServerWrapperPort : WrapperPort

        ClientIpAddress : IpAddress
        ClientTcpPort : TcpPort
        ClientWrapperPort : WrapperPort
    }

type UdpWrapperContext =
    {
        ServerIpAddress : IpAddress
        ServerUdpPort : UdpPort
        ServerWrapperPort : WrapperPort

        ClientIpAddress : IpAddress
        ClientUdpPort : UdpPort
        ClientWrapperPort : WrapperPort
    }

type TcpUdpIpContext =
    | Tcp of TcpWrapperContext
    | Udp of UdpWrapperContext