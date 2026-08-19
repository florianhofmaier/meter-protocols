namespace Metering.Mbus.Protocol.Records

open System
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling

type InfoBlockRaw =
    private InfoBlock of ReadOnlyMemory<byte>

module InfoBlockRaw =

    let bytes (InfoBlock raw) =
        raw

    let firstByte (InfoBlock raw) =
        raw.Span[0]

    let private maxBytes = 11

    let private parseBytes : Parser<ReadOnlyMemory<byte>> =

        let rec loop i acc = parser {
            if i >= maxBytes then
                return! fail "too many extension bytes in info block (limit 10)"

            let! b = parseU8
            let acc = b :: acc

            if (b &&& 0x80uy) <> 0uy then
                return! loop (i + 1) acc
            else
                return acc |> List.rev |> List.toArray |> ReadOnlyMemory
        }

        loop 0 []

    let parse : Parser<InfoBlockRaw> =
        parseBytes |>> InfoBlock