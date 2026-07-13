namespace Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp

open System

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type WrapperPduRaw =
    {
        Version : ParsedField<uint16>
        SourceWrapperPort : ParsedField<uint16>
        DestinationWrapperPort : ParsedField<uint16>
        DataLength : ParsedField<uint16>
        Data : ParsedField<ReadOnlyMemory<byte>>
    }

module WrapperPduRaw =

    let parse : Parser<ParsedField<WrapperPduRaw>> =
        parseField "TCP/UDP wrapper PDU" <|
            parser {
                let! version =
                    parseField "version" parseU16BigEndian

                let! sourceWrapperPort =
                    parseField "source-wPort" parseU16BigEndian

                let! destinationWrapperPort =
                    parseField "destination-wPort" parseU16BigEndian

                let! dataLength =
                    parseField "data-length" parseU16BigEndian

                let! data =
                    parseField "data" <|
                        take(int dataLength.Value)

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
        (rawVersion: ParsedField<uint16>)
        : Validation<unit> =

            ensure
                rawVersion
                $"unsupported wrapper version 0x{rawVersion.Value:X4}"
                (rawVersion.Value = 0x0001us)

    let fromRaw (raw: WrapperPduRaw) : Validation<WrapperPdu> =
        validator {
            do! validateVersion raw.Version

            return {
                SourceWrapperPort =
                    WrapperPort.create raw.SourceWrapperPort.Value

                DestinationWrapperPort =
                    WrapperPort.create raw.DestinationWrapperPort.Value

                Data = raw.Data.Value
            }
        }