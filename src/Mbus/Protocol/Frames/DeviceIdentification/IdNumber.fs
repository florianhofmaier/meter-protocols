namespace Metering.Mbus.Protocol.Frames.DeviceIdentification

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Utility

type IdNumberRaw =
    private RawIdNumber of uint32

type IdNumber =
    private
        IdNumber of uint32

module IdNumberRaw =

    let value (RawIdNumber value) =
        value

    let parse : Parser<ParsedField<IdNumberRaw>> =
        parseField "Identification Number" parseU32LittleEndian
        |>> ParsedField.map RawIdNumber

module IdNumber =

    let private digitCount = 8

    let value (IdNumber value) =
        value

    let toBcd (IdNumber value) =
        Bcd.encodeUInt32 digitCount value

    let fromRaw
        (raw: ParsedField<IdNumberRaw>)
        : Validation<IdNumber> =

        let value = IdNumberRaw.value raw.Value

        match Bcd.tryFindInvalidBcdNibble digitCount value with
        | Some (_, nibble) ->
            failed raw $"Invalid BCD nibble 0x{nibble:X} in identification number"

        | None ->
            value
            |> Bcd.decodeUInt32 digitCount
            |> IdNumber
            |> passed


