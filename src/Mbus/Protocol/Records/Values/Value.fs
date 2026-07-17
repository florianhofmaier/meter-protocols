namespace Metering.Mbus.Protocol.Records

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

    let fromRaw (raw: Field<ValueRaw>) : Validation<Value> =
        validator {
            match raw.Value with
            | ValueRaw.NoData ->
                return
                    Value.NoData

            | ValueRaw.Int8 v ->
                return!
                    Integer8Bit.fromBytes v
                    |> map Value.Int8

            | ValueRaw.Int16 v ->
                return!
                    Integer16Bit.fromBytes v
                    |> map Value.Int16

            | ValueRaw.Int24 v ->
                return!
                    Integer24Bit.fromBytes v
                    |> map Value.Int24

            | ValueRaw.Int32 v ->
                return!
                    Integer32Bit.fromBytes v
                    |> map Value.Int32

            | ValueRaw.Int48 v ->
                return!
                    Integer48Bit.fromBytes v
                    |> map Value.Int48

            | ValueRaw.Int64 v ->
                return!
                    Integer64Bit.fromBytes v
                    |> map Value.Int64

            | ValueRaw.Real32 v ->
                return!
                    Real32Bit.fromBytes v
                    |> map Value.Real32

            | ValueRaw.Bcd2Digit v ->
                return!
                    Bcd2Digit.fromBytes v
                    |> map Value.Bcd2Digit

            | ValueRaw.Bcd4Digit v ->
                return!
                    Bcd4Digit.fromBytes v
                    |> map Value.Bcd4Digit

            | ValueRaw.Bcd6Digit v ->
                return!
                    Bcd6Digit.fromBytes v
                    |> map Value.Bcd6Digit

            | ValueRaw.Bcd8Digit v ->
                return!
                    Bcd8Digit.fromBytes v
                    |> map Value.Bcd8Digit

            | ValueRaw.Bcd12Digit v ->
                return!
                    Bcd12Digit.fromBytes v
                    |> map Value.Bcd12Digit

            | ValueRaw.Text v ->
                return!
                    Text.fromBytes v
                    |> map Text

            | ValueRaw.PosBcd v ->
                return!
                    Bcd.fromBytes v
                    |> map Value.PosBcd

            | ValueRaw.NegBcd v ->
                return!
                    Bcd.fromBytes v
                    |> map int64
                    |> map (fun x -> -x)
                    |> map Value.NegBcd

            | ValueRaw.Binary v ->
                return
                    Binary v.Value

            | ValueRaw.SelectionForReadout ->
                return SelectionForReadout
        }
