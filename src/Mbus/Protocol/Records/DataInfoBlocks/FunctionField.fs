namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

type FunctionField =
    | Inst
    | Max
    | Min
    | Err

module FunctionField =

    let mask = 0x30uy
    let shift = 4
    let inst = 0x00uy
    let max = 0x01uy
    let min = 0x02uy
    let errorState = 0x03uy

    let (|IsFunc|_|) expected b =
        if (b &&& mask) >>> shift = expected then Some () else None

    let fromDif b =
        match b with
        | IsFunc inst -> Inst
        | IsFunc min -> Min
        | IsFunc max -> Max
        | IsFunc errorState -> Err
        | _ -> invalidOp $"invalid function: 0x{b:X2}"

    let toDif fn =
        match fn with
        | Inst -> (inst <<< shift) &&& mask
        | Min -> (min <<< shift) &&& mask
        | Max -> (max <<< shift) &&& mask
        | Err -> (errorState <<< shift) &&& mask