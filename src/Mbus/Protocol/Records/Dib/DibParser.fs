module Mbus.Records.DataInfoBlocks.DibParser

open Metering.Common.Decoding.Parsers.Core
open Metering.Mbus.Records.Dib


type private Dife = { StNum: uint64; Tariff: uint32; SubUnit: uint16 }

let private parseExtByte (acc: Dife) i b : Dife =
    let sn = acc.StNum ||| StorageNumber.fromDife b i
    let tn = acc.Tariff ||| Tariff.fromDife b i
    let su = acc.SubUnit ||| SubUnit.fromDife b i
    { StNum = sn; Tariff = tn; SubUnit = su }

let private parseDife dif stNumSeed : Parser<Dife> =
    parser {
        let seed = { StNum = stNumSeed; Tariff = 0u; SubUnit = 0us }
        if InfoBlock.isExtended dif then
            let! difeParsed = InfoBlock.parseExt seed parseExtByte
            return difeParsed
        else
            return seed
    }

let parse dif : Parser<MbusFunctionField * StorageNumber * Tariff * SubUnit> = parser {
    let fn = FunctionField.fromDif dif
    let stNumSeed = uint64 (StorageNumber.fromDif dif)
    let! dife = parseDife dif stNumSeed
    let! stNum =
        match StorageNumber.createFromU64 dife.StNum with
        | Ok v -> ok v
        | Error msg -> fail msg
    let! tariff =
        match Tariff.createFromU32 dife.Tariff with
        | Ok v -> ok v
        | Error msg -> fail msg
    let! subUnit =
        match SubUnit.createFromU16 dife.SubUnit with
        | Ok v -> ok v
        | Error msg -> fail msg
    return fn, stNum, tariff, subUnit
}
