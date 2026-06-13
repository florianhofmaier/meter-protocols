namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type ConfigurationRaw =
    private Configuration of uint16

module ConfigurationRaw =

    let value (Configuration v) = v

    let parse : Parser<ParsedField<ConfigurationRaw>> =
        parseField "Configuration" parseU16LittleEndian
        |>> ParsedField.map Configuration
