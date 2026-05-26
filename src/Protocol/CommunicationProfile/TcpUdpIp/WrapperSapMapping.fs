namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open Metering.Common.Decoding.Validators.Core

module WrapperServerSap =

    let fromWPort value : Validation<ServerSap> =
        match value with
        | 0x0000us -> passed ServerSap.ServerNoStation
        | 0x0001us -> passed ServerSap.ManagementLogicalDevice

        | value when 0x0002us <= value && value <= 0x000Fus ->
            failed $"server wrapper port 0x{value:X4} is reserved"

        | value when 0x0010us <= value && value <= 0x007Eus ->
            validationOk(ServerSap.AssignedLogicalDevice value)

        | 0x007Fus ->
            validationOkServerSap.AllStationBroadcast

        | other ->
            validationError $"invalid server wrapper port 0x{other:X4}"

    let toWPort value =
        match value with
        | ServerSap.ServerNoStation -> WrapperPort 0x0000us
        | ServerSap.ManagementLogicalDevice -> WrapperPort 0x0001us
        | ServerSap.AssignedLogicalDevice value -> WrapperPort value
        | ServerSap.AllStationBroadcast -> WrapperPort 0x007Fus

module WrapperClientSap =

    let fromWPort value : Validation<ClientSap> =
        match value with
        | 0x0000us ->validationOk ClientSap.ClientNoStation
        | 0x0001us ->validationOk ClientSap.ClientManagementProcess
        | 0x0010us ->validationOk ClientSap.PublicClient

        | value when
            (0x0002us <= value && value <= 0x000Fus)
            || (0x0011us <= value && value <= 0x00FFus) ->
            validationOk(ClientSap.AssignedClientApplicationProcess value)

        | other ->
            validationError $"invalid client wrapper port 0x{other:X4}"

    let toWPort value =
        match value with
        | ClientSap.ClientNoStation -> WrapperPort 0x0000us
        | ClientSap.ClientManagementProcess -> WrapperPort 0x0001us
        | ClientSap.PublicClient -> WrapperPort 0x0010us
        | ClientSap.AssignedClientApplicationProcess value -> WrapperPort value