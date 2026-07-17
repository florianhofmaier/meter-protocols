namespace Metering.Mbus.Protocol.Records

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Mbus.Protocol.Records

type DataRecordRaw =
    {
        Value: Field<ValueRaw>
        Dib: Field<InfoBlockRaw>
        Vib: Field<InfoBlockRaw>
    }

module DataRecordRaw =
    let parse: Parser<DataRecordRaw> =
        parser {
            let! dib =
                parseField "DIB" InfoBlockRaw.parse

            let! vib =
                parseField "VIB" InfoBlockRaw.parse

            let! value =
                parseField
                    "Data"
                    (ValueRaw.parse
                    <| InfoBlockRaw.firstByte dib.Value)

            return { Value = value; Dib = dib; Vib = vib }
        }

type SelectionRaw =
    {
        Dib: Field<InfoBlockRaw>
        Vib: Field<InfoBlockRaw>
    }

module SelectionRaw =

    let parse: Parser<SelectionRaw> =
        parser {
            let! dib =
                parseField "DIB" InfoBlockRaw.parse

            let! vib =
                parseField "VIB" InfoBlockRaw.parse

            return { Dib = dib; Vib = vib }
        }

type MfrSpecificDataRaw =
    {
        Mdh: Field<uint8>
        Data: Field<ReadOnlyMemory<byte>>
    }

type RecordRaw =
    | Data of Field<DataRecordRaw>
    | IdleFiller of Field<unit>
    | Selection of Field<SelectionRaw>
    | MfrData of Field<ReadOnlyMemory<byte>>
    | MfrDataMoreFollows of Field<ReadOnlyMemory<byte>>

module RecordRaw =

    let private maskSpecFn = 0x70uy
    let private shiftSpecFn = 4
    let private mfrData = 0x00uy
    let private mfrDataMoreFollows = 0x01uy
    let private idleFiller = 0x02uy
    let private globReadReq = 0x07uy

    let private isSpecFn b =
        (b &&& 0x0Fuy) = 0x0Fuy

    let private isSelection b =
        (b &&& 0x0Fuy) = 0x08uy

    let private specFnCode b =
        (b &&& maskSpecFn) >>> shiftSpecFn

    let private (|IsDataRecord|_|) b =
        if not (isSpecFn b) then Some b else None

    let private (|IsSelection|_|) b =
        if isSelection b then Some b else None

    let private (|IsMfrData|_|) b =
        if isSpecFn b && specFnCode b = mfrData then Some b else None

    let private (|IsMfrDataMoreFollows|_|) b =
        if isSpecFn b && specFnCode b = mfrDataMoreFollows then Some b else None

    let private (|IsIdleFiller|_|) b =
        if isSpecFn b && specFnCode b = idleFiller then Some b else None

    let private (|IsGlobReadReq|_|) b =
        if isSpecFn b && specFnCode b = globReadReq then Some b else None

    let parse: Parser<RecordRaw> = parser {
        let! dif = peekU8

        match dif with
        | IsDataRecord _ ->
            return!
                parseField "Data Record" DataRecordRaw.parse
                |>> Data

        | IsSelection _ ->
            return!
                parseField "SelectionForReadout" SelectionRaw.parse
                |>> Selection

        | IsIdleFiller _ ->
            return!
                parseField "Idle Filler" (skip 1)
                |>> IdleFiller

        | IsMfrData _ ->
            return!
                parseField "Manufacturer Specific Data" takeAll
                |>> MfrData

        | IsMfrDataMoreFollows _ ->
            return!
                parseField "Manufacturer Specific Data" takeAll
                |>> MfrDataMoreFollows

        | IsGlobReadReq _ ->
            return!
                failBefore 1 "global readout request not supported"

        | _ ->
            return!
                failBefore 1 $"invalid DIF: 0x{dif:X2}"
    }