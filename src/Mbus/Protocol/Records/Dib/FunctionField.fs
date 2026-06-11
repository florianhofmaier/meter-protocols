module Metering.Mbus.Records.Dib.FunctionField

open Metering.Mbus

let mask = 0x30uy
let shift = 4
let inst = 0x00uy
let min = 0x01uy
let max = 0x02uy
let errorState = 0x03uy

let (|IsFunc|_|) expected b =
    if (b &&& mask) >>> shift = expected then Some () else None

let fromDif b =
    match b with
    | IsFunc inst -> InstValue
    | IsFunc min -> MinValue
    | IsFunc max -> MaxValue
    | IsFunc errorState -> ValueInErrorState
    | _ -> invalidOp $"invalid function: 0x{b:X2}"

let toDif fn =
    match fn with
    | InstValue -> (inst <<< shift) &&& mask
    | MinValue -> (min <<< shift) &&& mask
    | MaxValue -> (max <<< shift) &&& mask
    | ValueInErrorState -> (errorState <<< shift) &&& mask