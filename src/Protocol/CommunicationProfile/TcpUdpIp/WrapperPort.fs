namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open Metering.Common.Decoding.Validators.Core

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
            validationOkClientNoStation

        | 0x0001us ->
            validationOkClientManagementProcess

        | 0x0010us ->
            validationOkPublicClient

        | value when isAssignedClientApplicationProcess value ->
            validationOk(AssignedClientApplicationProcess raw)

        | other ->
            validationError
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
            validationOkServerNoStation

        | 0x0001us ->
            validationOkManagementLogicalDevice

        | value when 0x0002us <= value && value <= 0x000Fus ->
            validationError
                $"server wrapper port 0x{value:X4} is reserved"

        | value when isAssignedLogicalDevice value ->
            validationOk(AssignedLogicalDevice raw)

        | 0x007Fus ->
            validationOkAllStationBroadcast

        | other ->
            validationError
                $"invalid server wrapper port 0x{other:X4}"