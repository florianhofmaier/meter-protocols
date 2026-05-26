module Metering.Dlms.Server.SupportingLayer.TcpUdpIp.ServerWrapperProfile

open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol.ApplicationLayer
open Metering.Dlms.Protocol.SupportingLayer.TcpUdpIp

let toXxDataIndication
    (raw: Parsed<WrapperPduRaw>)
    : Validation<XxDataIndication> =

    validator {
        let! pdu =
            WrapperPdu.fromRaw raw

        let! clientSap =
            Validation.parsed
                raw.Value.SourceWrapperPort
                WrapperClientSap.fromWPort

        and! serverSap =
            Validation.parsed
                raw.Value.DestinationWrapperPort
                WrapperServerSap.fromWPort

        return {
            Payload = {
                Value = pdu.Data
                Node = raw.Value.Data.Node
            }

            Address = {
                ClientSap = clientSap
                ServerSap = serverSap
            }
        }
    }

let fromXxDataRequest
    (request: XxDataRequest)
    : WrapperPdu =

    {
        SourceWrapperPort =
            request.Address.ServerSap
            |> WrapperServerSap.toWPort

        DestinationWrapperPort =
            request.Address.ClientSap
            |> WrapperClientSap.toWPort

        Data =
            request.Payload
    }
