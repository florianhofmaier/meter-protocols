namespace Metering.Mbus.Protocol.Records

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility

type ValueRaw =
    | NoData
    | Int8 of ParsedField<ReadOnlyMemory<byte>>
    | Int16 of ParsedField<ReadOnlyMemory<byte>>
    | Int24 of ParsedField<ReadOnlyMemory<byte>>
    | Int32 of ParsedField<ReadOnlyMemory<byte>>
    | Int48 of ParsedField<ReadOnlyMemory<byte>>
    | Int64 of ParsedField<ReadOnlyMemory<byte>>
    | Real32 of ParsedField<ReadOnlyMemory<byte>>
    | Bcd2Digit of ParsedField<ReadOnlyMemory<byte>>
    | Bcd4Digit of ParsedField<ReadOnlyMemory<byte>>
    | Bcd6Digit of ParsedField<ReadOnlyMemory<byte>>
    | Bcd8Digit of ParsedField<ReadOnlyMemory<byte>>
    | Bcd12Digit of ParsedField<ReadOnlyMemory<byte>>
    | Text of ParsedField<ReadOnlyMemory<byte>>
    | PosBcd of ParsedField<ReadOnlyMemory<byte>>
    | NegBcd of ParsedField<ReadOnlyMemory<byte>>
    | Binary of ParsedField<ReadOnlyMemory<byte>>
    | SelectionForReadout of unit

module ValueRaw =

    let private mask = 0x0Fuy
    let private noData = 0x00uy
    let private int8 = 0x01uy
    let private int16 = 0x02uy
    let private int24 = 0x03uy
    let private int32 = 0x04uy
    let private real32 = 0x05uy
    let private int48 = 0x06uy
    let private int64 = 0x07uy
    let private bcd2Digit = 0x09uy
    let private bcd4Digit = 0x0Auy
    let private bcd6Digit = 0x0Buy
    let private bcd8Digit = 0x0Cuy
    let private varLen = 0x0Duy
    let private bcd12Digit = 0x0Euy

    let private (|InRange|_|) min max value =
        if min <= value && value <= max then Some value else None


    let private parseVarLen =
        parser {
            let! b = parseField "LVAR" parseU8

            match int b.Value with
            | InRange 0 0xBF n ->
                return!
                    parseField "IEC 8859-1 String" (take n)
                    |>> Text

            | InRange 0xC0 0xC9 n ->
                return!
                    parseField "Positive BCD Number" (take ((n - 0xC0) * 2))
                    |>> PosBcd

            | InRange 0xD0 0xD9 n ->
                return!
                    parseField "Negative BCD Number" (take ((n - 0xD0) * 2))
                    |>> NegBcd

            | InRange 0xE0 0xEF n ->
                return!
                    parseField "Binary Number" (take (n - 0xE0))
                    |>> Binary

            | InRange 0xF0 0xF4 n ->
                return!
                    parseField "Binary Number" (take ((n - 0xEC) * 4))
                    |>> Binary

            | 0xF5 ->
                return!
                    parseField "Binary Number" (take 48)
                    |>> Binary

            | 0xF6 ->
                return!
                    parseField "Binary Number" (take 64)
                    |>> Binary

            | _ ->
                return!
                    failBefore 1 $"invalid LVAR: 0x{b:X2}"
        }

    let private (|IsDataType|_|) expected b =
        if b &&& mask = expected then Some () else None

    let parse b : Parser<ValueRaw> = parser {
        match b with
        | IsDataType noData ->
            return NoData

        | IsDataType int8 ->
            return!
                parseField "8 Bit Integer" (take 1) |>> Int8

        | IsDataType int16 ->
            return!
                parseField "16 Bit Integer" (take 2) |>> Int16

        | IsDataType int24 ->
            return!
                parseField "24 Bit Integer" (take 3) |>> Int24

        | IsDataType int32 ->
            return!
                parseField "32 Bit Integer" (take 4) |>> Int32

        | IsDataType int48 ->
            return!
                parseField "48 Bit Integer" (take 6) |>> Int48

        | IsDataType int64 ->
            return!
                parseField "64 Bit Integer" (take 8) |>> Int64

        | IsDataType real32 ->
            return!
                parseField "32 Bit Real" (take 4) |>> Real32

        | IsDataType bcd2Digit ->
            return!
                parseField "2 Digit BCD" (take 1) |>> Bcd2Digit

        | IsDataType bcd4Digit ->
            return!
                parseField "4 Digit BCD" (take 2) |>> Bcd4Digit

        | IsDataType bcd6Digit ->
            return!
                parseField "6 Digit BCD" (take 3) |>> Bcd6Digit

        | IsDataType bcd8Digit ->
            return!
                parseField "8 Digit BCD" (take 4) |>> Bcd8Digit

        | IsDataType bcd12Digit ->
            return!
                parseField "12 Digit BCD" (take 6) |>> Bcd12Digit

        | IsDataType varLen ->
            return!
                parseVarLen

        | _ -> return invalidOp $"invalid data field: 0x{b:X2}"
    }