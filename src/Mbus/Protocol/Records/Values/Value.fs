namespace Metering.Mbus.Protocol.Records

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records.Values

type Value =
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
    | Text of string
    | PosBcd of uint64
    | NegBcd of int64
    | Binary of ReadOnlyMemory<uint8>
    | SelectionForReadout

module Value =

    let validate (raw: Field<ValueRaw>) : Validation<Value> =
        validator {
            match raw.Value with
            | ValueRaw.NoData -> return Value.NoData

            | ValueRaw.Int8 v -> return! Integer8Bit.fromBytes v |> map Value.Int8

            | ValueRaw.Int16 v -> return! Integer16Bit.fromBytes v |> map Value.Int16

            | ValueRaw.Int24 v -> return! Integer24Bit.fromBytes v |> map Value.Int24

            | ValueRaw.Int32 v -> return! Integer32Bit.fromBytes v |> map Value.Int32

            | ValueRaw.Int48 v -> return! Integer48Bit.fromBytes v |> map Value.Int48

            | ValueRaw.Int64 v -> return! Integer64Bit.fromBytes v |> map Value.Int64

            | ValueRaw.Real32 v -> return! Real32Bit.fromBytes v |> map Value.Real32

            | ValueRaw.Bcd2Digit v -> return! Bcd2Digit.fromBytes v |> map Value.Bcd2Digit

            | ValueRaw.Bcd4Digit v -> return! Bcd4Digit.fromBytes v |> map Value.Bcd4Digit

            | ValueRaw.Bcd6Digit v -> return! Bcd6Digit.fromBytes v |> map Value.Bcd6Digit

            | ValueRaw.Bcd8Digit v -> return! Bcd8Digit.fromBytes v |> map Value.Bcd8Digit

            | ValueRaw.Bcd12Digit v -> return! Bcd12Digit.fromBytes v |> map Value.Bcd12Digit

            | ValueRaw.Text v -> return! Text.fromBytes v |> map Text

            | ValueRaw.PosBcd v -> return! Bcd.fromBytes v |> map Value.PosBcd

            | ValueRaw.NegBcd v ->
                return! Bcd.fromBytes v |> map int64 |> map (fun x -> -x) |> map Value.NegBcd

            | ValueRaw.Binary v -> return Binary v.Value

            | ValueRaw.SelectionForReadout -> return SelectionForReadout
        }

    /// Returns the DIF data-field bits for this representation.
    let dataField =
        function
        | NoData -> 0uy
        | Int8 _ -> 1uy
        | Int16 _ -> 2uy
        | Int24 _ -> 3uy
        | Int32 _ -> 4uy
        | Real32 _ -> 5uy
        | Int48 _ -> 6uy
        | Int64 _ -> 7uy
        | SelectionForReadout -> 8uy
        | Bcd2Digit _ -> 9uy
        | Bcd4Digit _ -> 10uy
        | Bcd6Digit _ -> 11uy
        | Bcd8Digit _ -> 12uy
        | Bcd12Digit _ -> 14uy
        | Text _
        | PosBcd _
        | NegBcd _
        | Binary _ -> 13uy

    /// Writes the value, including LVAR for variable-length representations.
    let write value : Writer<unit> =
        let invalid () =
            writerError "Value exceeds the range of its M-Bus representation"

        let bcd count (v: uint64) =
            writer {
                let mutable remaining = v

                for _ in 1..count do
                    let lo = remaining % 10UL
                    remaining <- remaining / 10UL
                    let hi = remaining % 10UL
                    remaining <- remaining / 10UL
                    do! writeU8 (byte (lo ||| (hi <<< 4)))
            }

        let variableBcd prefix v =
            writer {
                if v > 999999999999999999UL then
                    return! invalid ()

                let mutable count = 0
                let mutable remaining = v

                while remaining > 0UL do
                    count <- count + 1
                    remaining <- remaining / 100UL

                do! writeU8 (prefix + byte count)
                do! bcd count v
            }

        match value with
        | NoData
        | SelectionForReadout -> Writer.ret ()
        | Int8 v -> writeI8 v
        | Int16 v -> writeI16LittleEndian v
        | Int24 v when v >= -8388608 && v <= 8388607 -> writeI24LittleEndian v
        | Int24 _ -> invalid ()
        | Int32 v -> writeI32LittleEndian v
        | Int48 v when v >= -140737488355328L && v <= 140737488355327L -> writeI48LittleEndian v
        | Int48 _ -> invalid ()
        | Int64 v -> writeI64LittleEndian v
        | Real32 v -> writeF32LittleEndian v
        | Bcd2Digit v when v <= 99uy -> bcd 1 (uint64 v)
        | Bcd4Digit v when v <= 9999us -> bcd 2 (uint64 v)
        | Bcd6Digit v when v <= 999999u -> bcd 3 (uint64 v)
        | Bcd8Digit v when v <= 99999999u -> bcd 4 (uint64 v)
        | Bcd12Digit v when v <= 999999999999UL -> bcd 6 v
        | Bcd2Digit _
        | Bcd4Digit _
        | Bcd6Digit _
        | Bcd8Digit _
        | Bcd12Digit _ -> invalid ()
        | Text text when
            not (isNull text)
            && text.Length <= 191
            && Seq.forall (fun c -> int c <= 255) text
            ->
            writer {
                do! writeU8 (byte text.Length)
                do! writeMemory (ReadOnlyMemory(System.Text.Encoding.Latin1.GetBytes text))
            }
        | Text _ -> writerError "M-Bus text requires at most 191 ISO-8859-1 characters"
        | PosBcd v -> variableBcd 0xC0uy v
        | NegBcd v when v <= 0L && v >= -999999999999999999L -> variableBcd 0xD0uy (uint64 (-v))
        | NegBcd _ -> invalid ()
        | Binary bytes ->
            writer {
                let len = bytes.Length

                let code =
                    if len <= 15 then
                        Some(0xE0uy + byte len)
                    elif len <= 32 && len % 4 = 0 then
                        Some(0xECuy + byte (len / 4))
                    elif len = 48 then
                        Some 0xF5uy
                    elif len = 64 then
                        Some 0xF6uy
                    else
                        None

                match code with
                | None -> return! writerError $"Binary length {len} has no LVAR encoding"
                | Some code ->
                    do! writeU8 code
                    do! writeMemory bytes
            }
