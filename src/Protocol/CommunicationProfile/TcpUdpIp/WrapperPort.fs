namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open Metering.Common.Validators.Core

type WrapperPort =
    private
        WrapperPort of uint16

module WrapperPort =
    let create value = WrapperPort value

    let value (WrapperPort value) =
        value

type ClientWrapperPort =
    | ClientNoStation
    | ClientManagementProcess
    | PublicClient
    | AssignedClientApplicationProcess of WrapperPort

module ClientWrapperPort =
    let private isAssignedClientApplicationProcess value =
        (0x0002us <= value && value <= 0x000Fus)
        || (0x0011us <= value && value <= 0x00FFus)

    let fromRaw (raw: WrapperPort) : Validation<ClientWrapperPort> =
        let value = WrapperPort.value raw

        match value with
        | 0x0000us ->
            Validation.ok ClientNoStation

        | 0x0001us ->
            Validation.ok ClientManagementProcess

        | 0x0010us ->
            Validation.ok PublicClient

        | value when isAssignedClientApplicationProcess value ->
            Validation.ok (AssignedClientApplicationProcess raw)

        | other ->
            Validation.error
                $"invalid client wrapper port 0x{other:X4}"

type ServerWrapperPort =
    | ServerNoStation
    | ManagementLogicalDevice
    | AssignedLogicalDevice of WrapperPort
    | AllStationBroadcast

module ServerWrapperPort =
    let private isAssignedLogicalDevice value =
        0x0010us <= value && value <= 0x007Eus

    let fromRaw (raw: WrapperPort) : Validation<ServerWrapperPort> =
        let value = WrapperPort.value raw

        match value with
        | 0x0000us ->
            Validation.ok ServerNoStation

        | 0x0001us ->
            Validation.ok ManagementLogicalDevice

        | value when 0x0002us <= value && value <= 0x000Fus ->
            Validation.error
                $"server wrapper port 0x{value:X4} is reserved"

        | value when isAssignedLogicalDevice value ->
            Validation.ok (AssignedLogicalDevice raw)

        | 0x007Fus ->
            Validation.ok AllStationBroadcast

        | other ->
            Validation.error
                $"invalid server wrapper port 0x{other:X4}"