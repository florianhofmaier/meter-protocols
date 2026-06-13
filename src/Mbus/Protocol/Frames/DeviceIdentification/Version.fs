namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type VersionRaw =
    private Version of uint8

module VersionRaw =

    let parse : Parser<ParsedField<VersionRaw>> =
        parseField "Version" parseU8
        |>> ParsedField.map Version