module Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.SingleCharacterFrame

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

let parse : Parser<ParsedField<unit>> =
        parseField "Single Character Frame"
        <| expectU8 0xE5uy