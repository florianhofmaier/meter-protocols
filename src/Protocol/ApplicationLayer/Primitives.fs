namespace Metering.Dlms.Protocol.ApplicationLayer

open System
open Metering.Common.Parsers.ParserTree
open Metering.Dlms.Protocol.Acse.Fields

type ClientSap =
    | ClientNoStation
    | ClientManagementProcess
    | PublicClient
    | AssignedClientApplicationProcess of uint16

type ServerSap =
    | ServerNoStation
    | ManagementLogicalDevice
    | AssignedLogicalDevice of uint16
    | AllStationBroadcast

type CosemAddress =
    {
        ClientSap : ClientSap
        ServerSap : ServerSap
    }

type XxDataIndication =
    {
        Payload : ParsedField<ReadOnlyMemory<byte>>
        Address : CosemAddress
    }

type XxDataRequest =
    {
        Payload : ReadOnlyMemory<byte>
        Address : CosemAddress
    }

type PendingAssociation =
    {
        Address : CosemAddress
        OpenIndication : CosemOpenIndication
    }

type AssociationContext =
    {
        Address : CosemAddress
        ApplicationContext : ApplicationContextName
        Authentication : AuthenticationContext
        XdlmsContext : XdlmsContext
        SecurityContext : SecurityContext option
    }

type CfState =
    | Idle
    | AssociationPending of PendingAssociation
    | Associated of AssociationContext
    | AssociationReleasePending of AssociationContext