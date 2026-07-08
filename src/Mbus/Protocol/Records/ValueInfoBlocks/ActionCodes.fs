namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records

type ActionCode =
    | Set
    | AddValue
    | SubtractValue
    | Or
    | And
    | Xor
    | AndNot
    | Clear
    | AddEntry
    | DeleteEntry
    | DelayedAction
    | FreezeData
    | AddToReadoutList
    | DeleteFromReadoutList
    | Get

module ActionCode =

    let private table =
        Map [
            0x00uy, Set
            0x01uy, AddValue
            0x02uy, SubtractValue
            0x03uy, Or
            0x04uy, And
            0x05uy, Xor
            0x06uy, AndNot
            0x07uy, Clear
            0x08uy, AddEntry
            0x09uy, DeleteEntry
            0x0Auy, DelayedAction
            0x0Buy, FreezeData
            0x0Cuy, AddToReadoutList
            0x0Duy, DeleteFromReadoutList
            0xFFuy, Get
        ]

    let tryMap
        (bytes: ReadOnlyMemory<byte>)
        (pos: int)
        : ActionCode option * int =

        if pos >= bytes.Length then
            None, pos
        else
            match Map.tryFind bytes.Span[pos] table with
            | Some action -> Some action, pos + 1
            | None -> None, pos

    let map
        (infoBlock: ParsedField<InfoBlockRaw>)
        (pos: int)
        : Validation<ActionCode> * int =

        let bytes = InfoBlockRaw.bytes infoBlock.Value
        let action, nextPos = tryMap bytes pos

        match action with
        | Some value -> passed value, nextPos
        | None when pos < bytes.Length ->
            failed infoBlock $"Unknown action code 0x{bytes.Span[pos]:X2} at index {pos}", pos
        | None ->
            failed infoBlock $"Missing action code at index {pos}", pos
