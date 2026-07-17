namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type LField =
    private LField of uint8

module LField =

    let value (LField v) =
        v

    let parse : Parser<ParsedField<LField>> =
        parseField "Length Field"
        <| parser {
            let! value = parseU8
            do! expectU8 value

            return LField value
        }