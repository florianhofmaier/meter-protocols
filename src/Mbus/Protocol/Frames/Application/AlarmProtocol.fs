namespace Metering.Mbus.Protocol.Frames.Application

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type Alarms =
    private | Alarms of uint8

module Alarms =

    let value (Alarms value) =
        value

    let parse : Parser<ParsedField<Alarms>> =
        parseField "Alarms" parseU8
        |>> ParsedField.map Alarms

