namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type Alarms =
    private | Alarms of uint8

module Alarms =

    let value (Alarms value) =
        value

    let parse : Parser<Field<Alarms>> =
        parseField "Alarms" parseU8
        |>> Field.map Alarms

