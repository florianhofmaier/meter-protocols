namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type ConfigurationFieldExtensionRaw =
    | Mode7 of ParsedField<uint8>

module ConfigurationFieldExtensionRaw =

    let parse : Parser<ConfigurationFieldExtensionRaw> =
        parseField
            "Configuration Field Extension"
            parseU8
        |>> Mode7