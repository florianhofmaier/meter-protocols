namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type VersionRaw =
    private RawVersion of uint8

type Version =
    private Version of uint8

module VersionRaw =

    let value (RawVersion value) =
        value

    let parse : Parser<ParsedField<VersionRaw>> =
        parseField "Version" parseU8
        |>> ParsedField.map RawVersion

module Version =

    let value (Version value) =
        value

    let fromRaw
        (raw: ParsedField<VersionRaw>)
        : Validation<Version> =

        let value = VersionRaw.value raw.Value

        if value = 0xFFuy then
            failed raw "Wildcard byte 0xFF is not allowed in version identification"
        else
            passed (Version value)


