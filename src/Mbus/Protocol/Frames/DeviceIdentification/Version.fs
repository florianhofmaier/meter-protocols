namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type VersionRaw = private RawVersion of uint8

type Version = private Version of uint8

module VersionRaw =

    let value (RawVersion value) = value

    let parse: Parser<Field<VersionRaw>> =
        parseField "Version" parseU8 |>> Field.map RawVersion

module Version =

    let value (Version value) = value

    /// Creates a concrete version, excluding the FF wildcard (EN 13757-7, 7.5.3).
    let create value =
        if value < 0 || value > 254 then
            Error "Version must be between 0 and 254; 255 is a selection wildcard"
        else
            Ok(Version(byte value))

    let validate (raw: Field<VersionRaw>) : Validation<Field<Version>> =

        let value = VersionRaw.value raw.Value

        if value = 0xFFuy then
            failed raw "Wildcard byte 0xFF is not allowed in version identification"
        else
            raw |> Field.withValue (Version value) |> passed

    /// Writes the validated Version field.
    let write (field: Version) : Writer<unit> = writeU8 (value field)
