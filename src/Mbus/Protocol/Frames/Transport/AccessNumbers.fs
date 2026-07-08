namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type AccessNumberRaw =
    private AccessNumber of uint8

module AccessNumberRaw =

    let value (AccessNumber v) = v

    let parse : Parser<ParsedField<AccessNumberRaw>> =
        parseField "AccessNumber" parseU8
        |>> ParsedField.map AccessNumber

type AccessNumber =
    private AccessNumber of uint8

module AccessNumber =

    let value (AccessNumber v) =
        v

    let fromRaw (raw: ParsedField<AccessNumberRaw>) =
        passed (AccessNumber (AccessNumberRaw.value raw.Value))
