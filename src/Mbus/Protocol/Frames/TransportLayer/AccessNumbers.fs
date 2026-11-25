namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type AccessNumberRaw = private AccessNumber of uint8

module AccessNumberRaw =

    let value (AccessNumber v) = v

    let parse: Parser<Field<AccessNumberRaw>> =
        parseField "Access Number" parseU8 |>> Field.map AccessNumber

type AccessNumber = private AccessNumber of uint8

module AccessNumber =

    /// Creates an access number; every byte value is permitted.
    let create value = AccessNumber value

    let value (AccessNumber v) = v

    let validate (raw: Field<AccessNumberRaw>) : Validation<AccessNumber> =

        AccessNumber(AccessNumberRaw.value raw.Value) |> passed

    /// Writes the access number byte.
    let write (number: AccessNumber) : Writer<unit> = writeU8 (value number)
