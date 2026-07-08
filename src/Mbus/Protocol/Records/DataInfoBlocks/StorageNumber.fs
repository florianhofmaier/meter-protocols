namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

open System

type StorageNumber =
    private
        StorageNumber of uint64

module StorageNumber =
    let create (v: int) =
        let max = Int32.MaxValue
        match v with
        | _ when v < 0 -> Error $"Value for StorageNum must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for StorageNum must be in [0..{max}], got {v}"
        | _ -> v |> uint64 |> StorageNumber |> Ok

    let value (StorageNumber v) = v

    let zero = StorageNumber 0UL

    let private maskDif = 0x40uy

    let private shiftDif = 6

    let private maskDife = 0x0Fuy

    let inline private shiftDife i = i * 4 + 1

    let private fromDifByte b =
        (b &&& maskDif) >>> shiftDif
        |> uint64

    let private fromDifeByte b i =
        b &&& maskDife |> uint64 <<< shiftDife i

    let fromInfoBlock(bytes: ReadOnlyMemory<byte>) =
        Dife.foldBytes
            <| (fun acc b i -> acc ||| fromDifeByte b (i - 1))
            <| fromDifByte bytes.Span[0]
            <| bytes
        |> StorageNumber

    let private toDif stNum =
        let difBit = stNum &&& 0x01 |> byte
        (difBit <<< shiftDif) &&& maskDif

    let private toDifeByte stNum i =
        let mask = (int maskDife) <<< shiftDife i
        stNum &&& mask >>> shiftDife i |> byte