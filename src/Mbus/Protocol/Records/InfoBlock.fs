module Mbus.Records.InfoBlock

open Mbus.BaseWriters.BinaryWriters
open Mbus.BaseWriters.Core
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling

let isExtended b = (b &&& 0x80uy) <> 0uy
let maxExtBytes = 10

let parseExt seed f : Parser<'a> =
    let rec loop i acc =
        parser {
            if i >= maxExtBytes then return! fail "too many DIB/VIB bytes (limit 11)"
            let! b = parseU8
            let acc' = f acc i b
            if isExtended b then return! loop (i + 1) acc'
            else return acc'
        }
    loop 0 seed

let writeExt (seed: 'a) (f: 'a -> int -> byte) : Writer<unit> =
    let rec loop (i: int) : Writer<unit> =
        writer {
            if i >= maxExtBytes then
                return! writerError "too many DIB/VIB bytes (limit 11)"
            else
                let b = f seed i
                do! writeU8 b
                if isExtended b then
                    do! loop (i + 1)
        }
    loop 0
