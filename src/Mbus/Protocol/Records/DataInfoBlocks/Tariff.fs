namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

open System

type Tariff =
    private
        Tariff of uint32

module Tariff =
    let create (v : int) =
        let max = (1 <<< 20) - 1
        match v with
        | _ when v < 0 -> Error $"Value for Tariff must be non-negative, got {v}"
        | _ when v > max -> Error $"Value for Tariff must be in [0..{max}], got {v}"
        | _ -> uint32 v |> Tariff |> Ok

    let value (Tariff v) = v

    let zero = Tariff 0u

    let private mask = 0x30uy

    let inline private shift i = i * 2

    let private fromDifeByte b i =
        (b &&& mask) >>> 4 |> uint32 <<< (shift i)

    let fromBytes(bytes: ReadOnlyMemory<uint8>) =
        Dife.foldBytes
        <| (fun acc b i -> acc ||| fromDifeByte b (i - 1))
            <| 0u
            <| bytes
        |> Tariff

    let private toDifeByte tariff i =
        let mask = (int mask) <<< (shift i)
        tariff &&& mask >>> (shift i) |> byte