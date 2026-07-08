namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

open System

type SubUnit =
    private
        SubUnit of uint16

module SubUnit =
    let create v =
        let max = (1 <<< 10) - 1
        match v with
        | _ when v < 0 -> Error $"Value for SubUnit must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for SubUnit must be in [0..{max}], got {v}"
        | _ -> uint16 v |> SubUnit  |> Ok

    let value (SubUnit v) = v

    let zero = SubUnit 0us

    let private mask = 0x40uy

    let private fromDifeByte b i  =
        (b &&& mask) >>> 6 |> uint16 <<< i

    let fromBytes(bytes: ReadOnlyMemory<uint8>) =
        Dife.foldBytes
        <| (fun acc b i -> acc ||| fromDifeByte b (i - 1))
            <| 0us
            <| bytes
        |> SubUnit

    let private toDifeByte (subUnit: int) (i: int) : byte =
        let mask = (int mask) <<< i
        subUnit &&& mask >>> i |> byte