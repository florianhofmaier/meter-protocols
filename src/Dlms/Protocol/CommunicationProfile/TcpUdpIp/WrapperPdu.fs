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
        Version : Field<uint16>
        SourceWrapperPort : Field<uint16>
        DestinationWrapperPort : Field<uint16>
        DataLength : Field<uint16>
        Data : Field<ReadOnlyMemory<byte>>
    }

module WrapperPduRaw =

    let parse : Parser<Field<WrapperPduRaw>> =
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
        SourceWrapperPort : Field<WrapperPort>
        DestinationWrapperPort : Field<WrapperPort>
        Data : Field<ReadOnlyMemory<byte>>
    }

module WrapperPdu =

    let private validateVersion
        (rawVersion: Field<uint16>)
        : Validation<unit> =

            ensure
                rawVersion
                $"unsupported wrapper version 0x{rawVersion.Value:X4}"
                (rawVersion.Value = 0x0001us)

    let fromRaw
        (raw: Field<WrapperPduRaw>)
        : Validation<Field<WrapperPdu>> =

        validator {
            do! validateVersion raw.Value.Version

            return
                raw
                |> Field.withValue {
                    SourceWrapperPort =
                        raw.Value.SourceWrapperPort
                        |> Field.map WrapperPort.create

                    DestinationWrapperPort =
                        raw.Value.DestinationWrapperPort
                        |> Field.map WrapperPort.create

                    Data = raw.Value.Data
                }
        }
