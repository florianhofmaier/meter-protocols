module Metering.Dlms.Protocol.Axdr

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility

open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol.Utility

type UsageFlag =
    | NotUsed
    | Used

module UsageFlag =
    let parse : Parser<UsageFlag> =
        parser {
            let! value = parseU8

            match value with
            | 0x00uy -> return NotUsed
            | 0x01uy -> return Used
            | other -> return! failure $"invalid A-XDR usage flag 0x{other:X2}"
        }

type Optional<'a> =
    | Absent
    | Present of 'a

module Optional =

    let parse (p: Parser<'raw>) : Parser<Optional<'raw>> =
        parser {
            let! usage = UsageFlag.parse

            match usage with
            | NotUsed -> return Absent
            | Used -> return! p |>> Present
        }

    let validate
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<Optional<'raw>>)
        : Validation<'valid option> =

        validator {
            match raw.Value with
            | Absent ->
                return None

            | Present value ->
                let present =
                    {
                        Id = raw.Id
                        Span = raw.Span
                        Value = value
                    }

                let! valid =
                    validatePresent present

                do!
                    PresenceDiagnostic.emit raw presenceDiagnostic

                return Some valid
        }

    let validateField
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<Optional<'raw>>)
        : Validation<Field<'valid option>> =

        validator {
            let! value =
                validate presenceDiagnostic validatePresent raw

            return raw |> Field.withValue value
        }

    let toOption mapper value =
        match value with
        | Absent -> None
        | Present x -> Some (mapper x)

type Default<'a> =
    | Defaulted
    | Explicit of 'a

type ExplicitDefaultDiagnostic =
    | NoExplicitDefaultDiagnostic
    | InfoWhenExplicitDefault of string
    | WarningWhenExplicitDefault of string

module ExplicitDefaultDiagnostic =

    let emit
        (field: Field<_>)
        (diagnostic: ExplicitDefaultDiagnostic)
        : Validation<unit> =

        match diagnostic with
        | NoExplicitDefaultDiagnostic ->
            passed ()

        | InfoWhenExplicitDefault message ->
            info field message

        | WarningWhenExplicitDefault message ->
            warning field message

module Default =

    let parse
        (p: Parser<'a>)
        : Parser<Default<'a>> =

        parser {
            let! usage = UsageFlag.parse

            match usage with
            | NotUsed -> return Defaulted
            | Used -> return! p |>> Explicit
        }

    let valueOrDefault defaultValue value =
        match value with
        | Defaulted -> defaultValue
        | Explicit x -> x

    let validate
        (defaultValue: 'valid)
        (explicitDefaultDiagnostic: ExplicitDefaultDiagnostic)
        (validateExplicit: Field<'raw> -> Validation<'valid>)
        (raw: Field<Default<'raw>>)
        : Validation<'valid>
        when 'valid : equality =

        validator {
            match raw.Value with
            | Defaulted ->
                return defaultValue

            | Explicit value ->
                let explicitField: Field<'raw> =
                    {
                        Id = raw.Id
                        Span = raw.Span
                        Value = value
                    }

                let! valid =
                    validateExplicit explicitField

                do!
                    if valid = defaultValue then
                        ExplicitDefaultDiagnostic.emit raw explicitDefaultDiagnostic
                    else
                        passed ()

                return valid
        }

    let validateField
        (defaultValue: 'valid)
        (explicitDefaultDiagnostic: ExplicitDefaultDiagnostic)
        (validateExplicit: Field<'raw> -> Validation<'valid>)
        (raw: Field<Default<'raw>>)
        : Validation<Field<'valid>>
        when 'valid : equality =

        validator {
            let! value =
                validate defaultValue explicitDefaultDiagnostic validateExplicit raw

            return raw |> Field.withValue value
        }

type Boolean =
    private
        Boolean of bool

module Boolean =
    let create value = Boolean value
    let value (Boolean value) = value

    let parse : Parser<Boolean> =
        parser {
            let! value = parseU8

            match value with
            | 0x00uy -> return Boolean false
            | 0x01uy -> return Boolean true
            | other -> return! failure $"invalid A-XDR BOOLEAN value 0x{other:X2}"
        }

type Integer8 =
    private
        Integer8 of sbyte

module Integer8 =
    let create value = Integer8 value
    let value (Integer8 value) = value

    let parse : Parser<Integer8> =
        parseI8 |>> Integer8

type Unsigned8 =
    private
        Unsigned8 of byte

module Unsigned8 =
    let create value = Unsigned8 value
    let value (Unsigned8 value) = value

    let parse : Parser<Unsigned8> =
        parseU8 |>> Unsigned8

type Unsigned16 =
    private
        Unsigned16 of uint16

module Unsigned16 =
    let create value = Unsigned16 value
    let value (Unsigned16 value) = value

    let parse : Parser<Unsigned16> =
        parseU16BigEndian |>> Unsigned16

type Integer16 =
    private
        Integer16 of int16

module Integer16 =
    let create value = Integer16 value
    let value (Integer16 value) = value

    let parse : Parser<Integer16> =
        parseI16BigEndian |>> Integer16

type OctetString =
    private
        OctetString of ReadOnlyMemory<byte>

module OctetString =
    let create bytes =
        OctetString bytes

    let toBytes (OctetString bytes) =
        bytes

    let length (OctetString bytes) =
        bytes.Length

    let parse : Parser<OctetString> =
        parser {
            let! len = parseLength
            return! take len |>> OctetString
        }

    let parseContent
        (inner: Parser<'a>)
        : Parser<'a> =

        parser {
            let! length =
                parseLength

            return!
                runOnSubSlice length inner
        }
