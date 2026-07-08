module Metering.Mbus.Protocol.Records.ValueInfoBlocks.Utility

open System

let maskCode (bytes: ReadOnlyMemory<byte>) (pos: int) =
    bytes.Span[pos] &&& 0x7Fuy

let tryMap<'a>
    (table: Map<byte, 'a>)
    (bytes: ReadOnlyMemory<byte>)
    (pos: int)
    : 'a option * int =

    let code = maskCode bytes pos
    match Map.tryFind code table with
    | Some vif -> Some vif, pos + 1
    | None -> None, pos
