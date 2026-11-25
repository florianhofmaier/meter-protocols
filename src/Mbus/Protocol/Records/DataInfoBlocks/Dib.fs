namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records

type Dib = {
    Fn: FunctionField
    StNum: StorageNumber
    Tariff: Tariff
    SubUnit: SubUnit
}

module Dib =

    let validate (raw: Field<InfoBlockRaw>) : Validation<Dib> =

        let bytes = InfoBlockRaw.bytes raw.Value
        let dif = InfoBlockRaw.firstByte raw.Value

        passed {
            Fn = FunctionField.fromDif dif
            StNum = StorageNumber.fromInfoBlock bytes
            Tariff = Tariff.fromBytes bytes
            SubUnit = SubUnit.fromBytes bytes
        }

    /// Writes DIF and the minimum number of DIFEs for the selected data field.
    let write dataField (dib: Dib) : Writer<unit> =
        writer {
            if dataField > 14uy then
                return! writerError "Invalid DIF data field"

            let storage = StorageNumber.value dib.StNum
            let tariff = Tariff.value dib.Tariff
            let subUnit = SubUnit.value dib.SubUnit

            if storage >>> 41 <> 0UL || tariff >>> 20 <> 0u || subUnit >>> 10 <> 0us then
                return! writerError "DIB exceeds ten DIFEs"

            let mutable s = storage >>> 1
            let mutable t = tariff
            let mutable u = subUnit

            let dif =
                dataField ||| FunctionField.toDif dib.Fn ||| (byte (storage &&& 1UL) <<< 6)

            do! writeU8 (dif ||| (if s <> 0UL || t <> 0u || u <> 0us then 0x80uy else 0uy))

            while s <> 0UL || t <> 0u || u <> 0us do
                let b = byte (s &&& 15UL) ||| (byte (t &&& 3u) <<< 4) ||| (byte (u &&& 1us) <<< 6)
                s <- s >>> 4
                t <- t >>> 2
                u <- u >>> 1
                do! writeU8 (b ||| (if s <> 0UL || t <> 0u || u <> 0us then 0x80uy else 0uy))
        }
