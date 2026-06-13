namespace Metering.Mbus.Protocol.Frames.Dll

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type CrcRaw =
    private Crc of uint8

module CrcRaw =

    let parse : Parser<ParsedField<CrcRaw>> =
        parseField "CRC" parseU8
        |>> ParsedField.map Crc