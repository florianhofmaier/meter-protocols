namespace Metering.Dlms.Server.SupportingLayer.TcpUdpIp

open System
open Metering.Common.Validators
open Metering.Dlms.Protocol.ApplicationLayer

type ServerSupportingLayer =
    {
        ParseXxDataIndication :
            ReadOnlyMemory<byte> -> ValidationReport<XxDataIndication>

        WriteXxDataRequest :
            XxDataRequest -> ReadOnlyMemory<byte>
    }