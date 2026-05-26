namespace Mbus.Records.DataInfoBlocks

type SubUnit =
    private
        SubUnit of int

module SubUnit =
    let create v =
        let max = (1 <<< 10) - 1
        match v with
        | _ when v < 0 -> Error $"Value for SubUnit must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for SubUnit must be in [0..{max}], got {v}"
        | _ -> SubUnit v |> Ok

    let createFromU16 (v: uint16) =
        let max = uint16 ((1 <<< 10) - 1)
        match v with
        | _ when v > max -> Error $"Value for SubUnit must be in [0..{max}], got {v}"
        | _ -> SubUnit (int v) |> Ok

    let value (SubUnit v) = v

    let zero = SubUnit 0

    let private mask = 0x40uy

    let fromDife b i  =
        (b &&& mask) >>> 6 |> uint16 <<< i

    let toDife (subUnit: int) (i: int) : byte =
        let mask = (int mask) <<< i
        subUnit &&& mask >>> i |> byte