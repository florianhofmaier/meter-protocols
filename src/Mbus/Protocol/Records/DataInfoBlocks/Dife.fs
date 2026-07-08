module Metering.Mbus.Protocol.Records.DataInfoBlocks.Dife

open System

let foldBytes
    (folder: 'State -> byte -> int -> 'State)
    (state: 'State)
    (dibBytes: ReadOnlyMemory<byte>) =

    let span = dibBytes.Span
    let mutable acc = state

    if span.Length > 1 then
        for i = 1 to span.Length - 1 do
            acc <- folder acc span[i] i

    acc
