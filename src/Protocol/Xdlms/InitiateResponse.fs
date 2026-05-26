namespace Metering.Dlms.Protocol.Xdlms

open Metering.Common.Parsers.Core
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type InitiateResponseRawFields =
    {
        NegotiatedQualityOfService : Axdr.Optional<Axdr.Integer8>
        NegotiatedDlmsVersionNumber : Axdr.Unsigned8
        NegotiatedConformance : ConformanceRaw
        ServerMaxReceivePduSize : Axdr.Unsigned16
        VaaName : Axdr.Integer16
    }

module InitiateResponseRawFields =

    let parseBody: Parser<InitiateResponseRawFields> =
        parser {
            let! negotiatedQualityOfService =
                withCtx "NegotiatedQualityOfService" <|
                    Axdr.Optional.parse Axdr.Integer8.parse

            let! negotiatedDlmsVersionNumber =
                withCtx "NegotiatedDlmsVersionNumber" <|
                    Axdr.Unsigned8.parse

            let! negotiatedConformance =
                withCtx "NegotiatedConformance" <|
                    ConformanceRaw.parse

            let! serverMaxReceivePduSize =
                withCtx "ServerMaxReceivePduSize" <|
                    Axdr.Unsigned16.parse

            let! vaaName =
                withCtx "VaaName" <|
                    Axdr.Integer16.parse

            return {
                NegotiatedQualityOfService = negotiatedQualityOfService
                NegotiatedDlmsVersionNumber = negotiatedDlmsVersionNumber
                NegotiatedConformance = negotiatedConformance
                ServerMaxReceivePduSize = serverMaxReceivePduSize
                VaaName = vaaName
            }
        }

type NegotiatedQualityOfService =
    private NegotiatedQualityOfService of Axdr.Integer8

module NegotiatedQualityOfService =
    let create value =
        NegotiatedQualityOfService value

    let value (NegotiatedQualityOfService value) =
        value

type ServerMaxReceivePduSize =
    private ServerMaxReceivePduSize of Axdr.Unsigned16

module ServerMaxReceivePduSize =
    let create value =
        ServerMaxReceivePduSize value

    let value (ServerMaxReceivePduSize value) =
        value

type VaaName =
    private VaaName of Axdr.Integer16

module VaaName =
    let create value =
        VaaName value

    let value (VaaName value) =
        value

type InitiateResponseValidatedFields =
    {
        NegotiatedQualityOfService : NegotiatedQualityOfService option
        NegotiatedDlmsVersionNumber : DlmsVersionNumber
        NegotiatedConformance : Conformance
        ServerMaxReceivePduSize : ServerMaxReceivePduSize
        VaaName : VaaName
    }

module InitiateResponseValidatedFields =
    let fromRaw (raw: InitiateResponseRawFields) : Validation<InitiateResponseValidatedFields> =
        validator {
            let negotiatedQualityOfService =
                raw.NegotiatedQualityOfService
                |> Axdr.Optional.toOption NegotiatedQualityOfService.create

            let negotiatedDlmsVersionNumber =
                raw.NegotiatedDlmsVersionNumber
                |> DlmsVersionNumber.create

            let serverMaxReceivePduSize =
                raw.ServerMaxReceivePduSize
                |> ServerMaxReceivePduSize.create

            let vaaName =
                raw.VaaName
                |> VaaName.create

            let! negotiatedConformance =
                Conformance.validate raw.NegotiatedConformance

            return {
                NegotiatedQualityOfService = negotiatedQualityOfService
                NegotiatedDlmsVersionNumber = negotiatedDlmsVersionNumber
                NegotiatedConformance = negotiatedConformance
                ServerMaxReceivePduSize = serverMaxReceivePduSize
                VaaName = vaaName
            }
        }