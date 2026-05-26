namespace Mbus.Records.DataInfoBlocks

open System

type StorageNumber =
    private
        StorageNumber of int

module StorageNumber =
    let create (v: int) =
        let max = Int32.MaxValue
        match v with
        | _ when v < 0 -> Error $"Value for StorageNum must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for StorageNum must be in [0..{max}], got {v}"
        | _ -> StorageNumber v |> Ok

    let createFromU64 (v: uint64) =
        let max = uint64 Int32.MaxValue
        match v with
        | _ when v > max -> Error $"Value for StorageNum must be in [0..{max}], got {v}"
        | _ -> StorageNumber (int v) |> Ok

    let value (StorageNumber v) = v

    let zero = StorageNumber 0

    let private maskDif = 0x40uy

    let private shiftDif = 6

    let private maskDife = 0x0Fuy

    let inline private shiftDife i = i * 4 + 1

    let fromDif b =
        (b &&& maskDif) >>> shiftDif

    let toDif stNum =
        let difBit = stNum &&& 0x01 |> byte
        (difBit <<< shiftDif) &&& maskDif

    let fromDife b i =
        b &&& maskDife |> uint64 <<< shiftDife i

    let toDife stNum i =
        let mask = (int maskDife) <<< shiftDife i
        stNum &&& mask >>> shiftDife i |> byte