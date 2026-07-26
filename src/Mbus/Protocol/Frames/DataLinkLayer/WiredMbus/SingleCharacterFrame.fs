module Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.SingleCharacterFrame

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

let parse : Parser<Field<unit>> =
    parseField "Format FT 1.2 Single Character"
    <| expectU8 0xE5uy