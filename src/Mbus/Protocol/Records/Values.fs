namespace Mbus.Records

open System
open System.Buffers.Binary
open System.Text

open Mbus.BaseWriters.BcdWriters
open Mbus.BaseWriters.BinaryWriters
open Mbus.BaseWriters.Core
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility
open Metering.Mbus.Protocol.Utility.BcdParsers

type MbusValue =
    | NoData
    | Int8 of int8
    | Int16 of int16
    | Int24 of int32
    | Int32 of int32
    | Int48 of int64
    | Int64 of int64
    | Real32 of float32
    | Bcd2Digit of uint8
    | Bcd4Digit of uint16
    | Bcd6Digit of uint32
    | Bcd8Digit of uint32
    | Bcd12Digit of uint64
    | VarLen of string

module MbusValue =
    let private mask = 0x0Fuy
    let private noData = 0x00uy
    let private int8 = 0x01uy
    let private int16 = 0x02uy
    let private int24 = 0x03uy
    let private int32 = 0x04uy
    let private real32 = 0x05uy
    let private int48 = 0x06uy
    let private int64 = 0x07uy
    let private select = 0x08uy
    let private bcd2Digit = 0x09uy
    let private bcd4Digit = 0x0Auy
    let private bcd6Digit = 0x0Buy
    let private bcd8Digit = 0x0Cuy
    let private varLen = 0x0Duy
    let private bcd12Digit = 0x0Euy

    let private (|IsDataType|_|) expected b =
        if b &&& mask = expected then Some () else None

    let private parseLvar : Parser<int> =
        parser {
            let! l = parseU8

            if l < 0xC0uy then
                return int l
            else
                return! fail $"unsupported LVAR: 0x{l:X2}"
        }

    let private parseVarLen : Parser<string> =
        parser {
            let! n = parseLvar
            let! bytes = take n
            let arr = bytes.ToArray()
            Array.Reverse(arr)

            return Encoding.UTF8.GetString(arr)
        }

    let private parseFloat32 : Parser<float32> =
        parser {
            let! b = take 4
            return BinaryPrimitives.ReadSingleLittleEndian b.Span
        }

    let parse b : Parser<MbusValue> = parser {
        match b with
        | IsDataType noData -> return NoData
        | IsDataType int8 -> return! parseI8 |>> Int8
        | IsDataType int16 -> return! parseI16LittleEndian |>> Int16
        | IsDataType int24 -> return! parseI24LittleEndian |>> Int24
        | IsDataType int32 -> return! parseI32LittleEndian |>> Int32
        | IsDataType int48 -> return! parseI48LittleEndian |>> Int48
        | IsDataType int64 -> return! parseI64LittleEndian |>> Int64
        | IsDataType real32 -> return! parseFloat32 |>> Real32
        | IsDataType bcd2Digit -> return! parseBcd2Digit |>> Bcd2Digit
        | IsDataType bcd4Digit -> return! parseBcd4Digit |>> Bcd4Digit
        | IsDataType bcd6Digit -> return! parseBcd6Digit |>> Bcd6Digit
        | IsDataType bcd8Digit -> return! parseBcd8Digit |>> Bcd8Digit
        | IsDataType bcd12Digit -> return! parseBcd12Digit |>> Bcd12Digit
        | IsDataType varLen -> return! parseVarLen |>> VarLen
        | IsDataType select -> return! fail "data field 'Selection for readout' not supported"
        | _ -> return invalidOp $"invalid data field: 0x{b:X2}"
    }

    let toDif v =
        match v with
        | NoData -> noData
        | Int8 _ -> int8
        | Int16 _ -> int16
        | Int24 _ -> int24
        | Int32 _ -> int32
        | Int48 _ -> int48
        | Int64 _ -> int64
        | Real32 _ -> real32
        | Bcd2Digit _ -> bcd2Digit
        | Bcd4Digit _ -> bcd4Digit
        | Bcd6Digit _ -> bcd6Digit
        | Bcd8Digit _ -> bcd8Digit
        | Bcd12Digit _ -> bcd12Digit
        | VarLen _ -> varLen

    let private writeReal32 (x: float32) : Writer<unit> =
        writer {
            let bytes = BitConverter.GetBytes(x)
            if not BitConverter.IsLittleEndian then Array.Reverse(bytes)
            for b in bytes do
                do! writeU8 b
        }

    let private writeVarLen (txt: string) : Writer<unit> =
        writer {
            let raw = Encoding.UTF8.GetBytes(txt)
            if raw.Length > 0xBF then
                return! fun st -> err st $"VarLen too long for supported LVAR (len={raw.Length})"
            else
                let len = byte raw.Length
                do! writeU8 len
                let rev = Array.rev raw
                for b in rev do
                    do! writeU8 b
        }

    let write (value: MbusValue) : Writer<unit> =
        writer {
            match value with
            | NoData -> return ()
            | Int8 x -> do! writeI8 x
            | Int16 x -> do! writeI16 x
            | Int24 x -> do! writeI24 x
            | Int32 x -> do! writeI32 x
            | Int48 x -> do! writeI48 x
            | Int64 x -> do! writeI64 x
            | Real32 x -> do! writeReal32 x
            | Bcd2Digit x -> do! writeBcdU8 x
            | Bcd4Digit x -> do! writeBcdU16 x
            | Bcd6Digit x -> do! writeBcdU24 x
            | Bcd8Digit x -> do! writeBcdU32 x
            | Bcd12Digit x -> do! writeBcdU48 x
            | VarLen txt -> do! writeVarLen txt
        }
