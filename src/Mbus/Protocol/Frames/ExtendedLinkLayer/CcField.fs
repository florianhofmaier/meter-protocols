namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type CcFieldRaw =
    private CcField of uint8

module CcFieldRaw =

    let parse: Parser<Field<CcFieldRaw>> =
        parseField "CC-Field" parseU8
        |>> Field.map CcField