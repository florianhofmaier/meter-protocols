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

    let parse : Parser<Field<VersionRaw>> =
        parseField "Version" parseU8
        |>> Field.map RawVersion

module Version =

    let value (Version value) =
        value

    let fromRaw
        (raw: Field<VersionRaw>)
        : Validation<Field<Version>> =

        let value = VersionRaw.value raw.Value

        if value = 0xFFuy then
            failed raw "Wildcard byte 0xFF is not allowed in version identification"
        else
            raw
            |> Field.withValue (Version value)
            |> passed

