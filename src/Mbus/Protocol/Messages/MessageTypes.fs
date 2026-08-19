namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Records

type LinkLayerReset =
    {
        Address: AField
    }

type Selection =
    {
        Selection: SelectionOfDevice
    }

type PrimaryAddress =
    | Unconfigured
    | PrimAdr of PrimAdr

type ChangePrimaryAddress =
    {
        OldAddress: AField
        NewAddress: PrimaryAddress
    }

type RequestUserDataAddress =
    | Unconfigured
    | Configured of PrimAdr
    | SelectionOfDevice
    | Diagnosis

type RequestUserData =
    {
        Fcb: bool
        Address: RequestUserDataAddress
    }

type RequestAlarms =
    {
        Fcb: bool
        Address: RequestUserDataAddress
    }

type MeterAddress =
    {
        IdNum: IdNumber
        Mfr: Manufacturer
        Version: Version
        DevType: DeviceType
    }

type PrimaryStationMessage =
    | LinkLayerReset of LinkLayerReset
    | Selection of Selection
    | ChangePrimaryAddress of ChangePrimaryAddress
    | RequestUserData of RequestUserData
    | RequestAlarms of RequestAlarms

type ResponseUserData =
    {
        Acd: bool
        Dfc: bool
        Status: StatusByte
        AccessNumber: AccessNumber
        MeterAddress: MeterAddress
        UserData: DataRecordRsp seq
        MfrData: MfrSpecificData option
        MoreFollows: bool
    }

type ResponseAlarms =
    {
        Acd: bool
        Dfc: bool
        Status: StatusByte
        AccessNumber: AccessNumber
        MeterAddress: MeterAddress
        Alarms: Alarms
    }

type SecondaryStationMessage =
    | ResponseUserData of ResponseUserData
    | ResponseAlarms of ResponseAlarms

type Message =
    | PrimaryStationMessage of PrimaryStationMessage
    | SecondaryStationMessage of SecondaryStationMessage
