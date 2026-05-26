module Metering.Dlms.Protocol.Axdr

open System
open Metering.Common.Parsers.BaseParsers
open Metering.Common.Parsers.BinaryParsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators
open Metering.Common.Validators.Core
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
            | other -> return! fail $"invalid A-XDR usage flag 0x{other:X2}"
        }

type Optional<'a> =
    | Absent
    | Present of 'a

module Optional =

    let parse (p: Parser<'a>) : Parser<Optional<'a>> =
        parser {
            let! usage = UsageFlag.parse

            match usage with
            | NotUsed -> return Absent
            | Used -> return! p |>> Present
        }

    let validateParsed
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: 'raw -> Validation<'valid>)
        (raw: Parsed<Optional<'raw>>)
        : Validation<'valid option> =

        Validation.withNode raw.Node <|
            match raw.Value with
            | Absent ->
                Validation.ok None

            | Present value ->
                validator {
                    let! valid =
                        validatePresent value

                    do!
                        PresenceDiagnostic.emit presenceDiagnostic

                    return Some valid
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

    let emit diagnostic : Validation<unit> =
        match diagnostic with
        | NoExplicitDefaultDiagnostic ->
            Validation.ok ()

        | InfoWhenExplicitDefault message ->
            Validation.info message

        | WarningWhenExplicitDefault message ->
            Validation.warning message

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

    let validateParsed
        (defaultValue: 'valid)
        (explicitDefaultDiagnostic: ExplicitDefaultDiagnostic)
        (validateExplicit: 'raw -> Validation<'valid>)
        (raw: Parsed<Default<'raw>>)
        : Validation<'valid>
        when 'valid : equality =

        Validation.withNode raw.Node <|
            match raw.Value with
            | Defaulted ->
                Validation.ok defaultValue

            | Explicit value ->
                validator {
                    let! valid =
                        validateExplicit value

                    do!
                        if valid = defaultValue then
                            ExplicitDefaultDiagnostic.emit explicitDefaultDiagnostic
                        else
                            Validation.ok ()

                    return valid
                }

module Required =

    let validateParsed
        (validateValue: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw>)
        : Validation<'valid> =

        Validation.withNode raw.Node <|
            validateValue raw.Value

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
            | other -> return! fail $"invalid A-XDR BOOLEAN value 0x{other:X2}"
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

    let parse : Parser<OctetString> =
        parser {
            let! len = parseLength
            return! takeMem len |>> OctetString
        }

    let parseAsBufferSlice : Parser<BufferSlice> =
        parser {
            let! len = parseLength
            return! BufferSlice.parse len
        }