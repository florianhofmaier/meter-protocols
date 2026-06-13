namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type IdNumberRaw =
    private IdNumber of uint32

module IdNumberRaw =

    let parse : Parser<ParsedField<IdNumberRaw>> =
        parseField "Identification Number" parseU32LittleEndian
        |>> ParsedField.map IdNumber