namespace Mbus.Records.DataInfoBlocks

type Tariff =
    private
        Tariff of int

module Tariff =
    let create (v : int) =
        let max = (1 <<< 20) - 1
        match v with
        | _ when v < 0 -> Error $"Value for Tariff must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for Tariff must be in [0..{max}], got {v}"
        | _ -> Tariff v |> Ok

    let createFromU32 (v: uint32) =
        let max = uint32 ((1 <<< 20) - 1)
        match v with
        | _ when v > max -> Error $"Value for Tariff must be in [0..{max}], got {v}"
        | _ -> Tariff (int v) |> Ok

    let value (Tariff v) = v

    let zero = Tariff 0

    let private mask = 0x30uy

    let inline private shift i = i * 2

    let fromDife b i =
        (b &&& mask) >>> 4 |> uint32 <<< (shift i)

    let toDife tariff i =
        let mask = (int mask) <<< (shift i)
        tariff &&& mask >>> (shift i) |> byte