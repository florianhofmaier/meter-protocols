module DlmsMessages.Axdr

open System
open DlmsMessages.Utility
open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core

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

    let toOption mapper value =
        match value with
        | Absent -> None
        | Present x -> Some (mapper x)

type Default<'a> =
    | Defaulted
    | Explicit of 'a

module Default =
    let parse (p: Parser<'a>) : Parser<Default<'a>> =
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

type Boolean = private Boolean of bool

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

type Integer8 = private Integer8 of sbyte

module Integer8 =
    let create value = Integer8 value
    let value (Integer8 value) = value

    let parse : Parser<Integer8> =
        parseI8 |>> Integer8

type Unsigned8 = private Unsigned8 of byte

module Unsigned8 =
    let create value = Unsigned8 value
    let value (Unsigned8 value) = value

    let parse : Parser<Unsigned8> =
        parseU8 |>> Unsigned8

type Unsigned16 = private Unsigned16 of uint16

module Unsigned16 =
    let create value = Unsigned16 value
    let value (Unsigned16 value) = value

    let parse : Parser<Unsigned16> =
        parseU16BigEndian |>> Unsigned16

type Integer16 = private Integer16 of int16

module Integer16 =
    let create value = Integer16 value
    let value (Integer16 value) = value

    let parse : Parser<Integer16> =
        parseI16BigEndian |>> Integer16

type OctetString = private OctetString of BufferSlice

module OctetString =
    let fromBufferSlice bufferSlice =
        OctetString bufferSlice

    let toBytes (OctetString bytes) =
        BufferSlice.slice bytes

    let toBufferSlice (OctetString bufferSlice) =
        bufferSlice

    let parseContent len: Parser<OctetString> =
        parser {
            let! start = pos
            let! bytes = getBuffer
            return fromBufferSlice <| BufferSlice.create bytes start len
        }

    let parse : Parser<OctetString> =
        parser {
            let! len = parseLength
            return! parseContent len
        }