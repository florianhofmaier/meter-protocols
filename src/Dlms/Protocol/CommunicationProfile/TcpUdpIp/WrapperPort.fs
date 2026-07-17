namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open Metering.Common.Decoding.Parsers
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

    let fromRaw
        (raw: Field<WrapperPort>)
        : Validation<Field<ClientWrapperPort>> =

        let value = WrapperPort.value raw.Value

        match value with
        | 0x0000us ->
            raw |> Field.withValue ClientNoStation |> passed

        | 0x0001us ->
            raw |> Field.withValue ClientManagementProcess |> passed

        | 0x0010us ->
            raw |> Field.withValue PublicClient |> passed

        | value when isAssignedClientApplicationProcess value ->
            raw
            |> Field.withValue (AssignedClientApplicationProcess raw.Value)
            |> passed

        | other ->
            failed raw $"invalid client wrapper port 0x{other:X4}"

type ServerWrapperPort =
    | ServerNoStation
    | ManagementLogicalDevice
    | AssignedLogicalDevice of WrapperPort
    | AllStationBroadcast

module ServerWrapperPort =
    let private isAssignedLogicalDevice value =
        0x0010us <= value && value <= 0x007Eus

    let fromRaw
        (raw: Field<WrapperPort>)
        : Validation<Field<ServerWrapperPort>> =

        let value = WrapperPort.value raw.Value

        match value with
        | 0x0000us ->
            raw |> Field.withValue ServerNoStation |> passed

        | 0x0001us ->
            raw |> Field.withValue ManagementLogicalDevice |> passed

        | value when 0x0002us <= value && value <= 0x000Fus ->
            failed raw $"server wrapper port 0x{value:X4} is reserved"

        | value when isAssignedLogicalDevice value ->
            raw
            |> Field.withValue (AssignedLogicalDevice raw.Value)
            |> passed

        | 0x007Fus ->
            raw |> Field.withValue AllStationBroadcast |> passed

        | other ->
            failed raw $"invalid server wrapper port 0x{other:X4}"
