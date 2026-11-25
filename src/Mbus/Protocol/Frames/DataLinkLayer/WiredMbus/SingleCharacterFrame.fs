module Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.SingleCharacterFrame

open Metering.Common.Decoding.Parsers
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

let parse: Parser<Field<unit>> =
    parseField "Format FT 1.2 Single Character" <| expectU8 0xE5uy

/// Writes the FT 1.2 acknowledgement character; IEC 60870-5-2, 3.2, p. 14.
let write: Writer<unit> = writeU8 0xE5uy

/// EN 13757-2:2024, 3.1: negative acknowledgement of an unsupported SND-UD2.
let writeNegative: Writer<unit> = writeU8 0xA2uy
