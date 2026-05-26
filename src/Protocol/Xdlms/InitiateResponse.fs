namespace Metering.Dlms.Protocol.Xdlms

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Dlms.Protocol

type InitiateResponseRawFields =
    {
        NegotiatedQualityOfService : ParsedField<Axdr.Optional<Axdr.Integer8>>
        NegotiatedDlmsVersionNumber : ParsedField<Axdr.Unsigned8>
        NegotiatedConformance : ParsedField<ConformanceRaw>
        ServerMaxReceivePduSize : ParsedField<Axdr.Unsigned16>
        VaaName : ParsedField<Axdr.Integer16>
    }

module InitiateResponseRawFields =

    let parseBody: Parser<InitiateResponseRawFields> =
        parser {
            let! negotiatedQualityOfService =
                parseField "NegotiatedQualityOfService" <|
                    Axdr.Optional.parse Axdr.Integer8.parse

            let! negotiatedDlmsVersionNumber =
                parseField "NegotiatedDlmsVersionNumber" <|
                    Axdr.Unsigned8.parse

            let! negotiatedConformance =
                parseField "NegotiatedConformance" <|
                    ConformanceRaw.parse

            let! serverMaxReceivePduSize =
                parseField "ServerMaxReceivePduSize" <|
                    Axdr.Unsigned16.parse

            let! vaaName =
                parseField "VaaName" <|
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