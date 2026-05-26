namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open System

open Metering.Common.Parsers.BaseParsers
open Metering.Common.Parsers.BinaryParsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core

type WrapperPduRaw =
    {
        Version : Parsed<uint16>
        SourceWrapperPort : Parsed<uint16>
        DestinationWrapperPort : Parsed<uint16>
        DataLength : Parsed<uint16>
        Data : Parsed<ReadOnlyMemory<byte>>
    }

module WrapperPduRaw =

    let parse : Parser<Parsed<WrapperPduRaw>> =
        parseNode "TCP/UDP wrapper PDU" <|
            parser {
                let! version =
                    parseNode "version" parseU16BigEndian

                let! sourceWrapperPort =
                    parseNode "source-wPort" parseU16BigEndian

                let! destinationWrapperPort =
                    parseNode "destination-wPort" parseU16BigEndian

                let! dataLength =
                    parseNode "data-length" parseU16BigEndian

                let! data =
                    parseNode "data" <|
                        takeMem (int dataLength.Value)

                return {
                    Version = version
                    SourceWrapperPort = sourceWrapperPort
                    DestinationWrapperPort = destinationWrapperPort
                    DataLength = dataLength
                    Data = data
                }
            }

type WrapperPdu =
    {
        SourceWrapperPort : WrapperPort
        DestinationWrapperPort : WrapperPort
        Data : ReadOnlyMemory<byte>
    }

module WrapperPdu =

    let private validateVersion
        (version: uint16)
        : Validation<unit> =

            Validation.ensure
                $"unsupported wrapper version 0x{version:X4}"
                (version = 0x0001us)

    let fromRaw (raw: WrapperPduRaw) : Validation<WrapperPdu> =
        validator {
            do!
                Validation.parsed
                    raw.Version
                    validateVersion

            return {
                SourceWrapperPort =
                    WrapperPort.create raw.SourceWrapperPort.Value

                DestinationWrapperPort =
                    WrapperPort.create raw.DestinationWrapperPort.Value

                Data = raw.Data.Value
            }
        }