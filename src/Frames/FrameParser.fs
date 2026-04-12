module Mbus.Frames.FrameParser

open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core
open Mbus.Records
open Mbus.Frames

let parseConfirmation : Parser<Frame> =
    (parser {
        do! expectU8 Frame.confirmationStartByte "invalid CNF frame"
        return Confirmation
    }) |> withCtx "single character frame (CNF)"

let parseShortFrame : Parser<ShortFrame> =
    (parser {
        do! expectU8 Frame.shortFrameStartByte "invalid start byte"
        let! cField = parseU8
        let! prmAdr = parseU8
        do! expectU8 (cField + prmAdr) "invalid checksum"
        do! expectU8 Frame.stopByte "invalid stop byte"
        return { CField = cField; PrmAdr = prmAdr }
    }) |> withCtx "short frame"

let private parseLongFrameLen : Parser<int> =
    parser {
        let! l1 = parseU8
        do! expectU8 l1 "length byte mismatch"
        return int l1
    }

let private parseRecords aplLen : Parser<RspRecord list> =
    (parser {
        let rec loop acc remaining =
            parser {
                if remaining = 0 then
                    return List.rev acc
                else
                    let! startPos = pos
                    let! record = Record.Parser.parseRspRec
                    let! endPos = pos
                    let consumed = endPos - startPos
                    return! loop (record :: acc) (remaining - consumed)
            }
        return! loop [ ] aplLen
    }) |> (withCtx "parsing APL records")

let checkCrc start l : Parser<unit> =
    parser {
        let! buf = getBuffer
        let expectedCrc = buf.Slice(start, l) |> Frame.calcCrc
        do! expectU8 expectedCrc "invalid checksum"
    }

let parseLongFrameHeader : Parser<int * int> =
    parser {
        do! expectU8 Frame.longFrameStartByte "invalid start byte"
        let! len = parseLongFrameLen
        do! expectU8 Frame.longFrameStartByte "invalid start byte"
        let! pos = pos
        return len, pos
    }

let private parseApl tpl lenUd : Parser<Apl> = parser {
    let aplLen = lenUd - Tpl.getLen tpl - 2 // minus CField and PrmAdr
    if aplLen < 0 then
        return! fail $"invalid APL length: {aplLen}"
    else
        return! AplParser.parseAny aplLen tpl
}

let parseLongFrame : Parser<LongFrame> =
    (parser {
        let! lenUd, startUd = parseLongFrameHeader
        let! cField = parseU8
        let! prmAdr = parseU8
        let! tpl = TplParser.parseAny
        let! apl = parseApl tpl lenUd
        do! checkCrc startUd lenUd
        do! expectU8 Frame.stopByte "invalid stop byte"
        return { CField = cField; PrmAdr = prmAdr; Tpl = tpl; Apl = apl }
    }) |> (withCtx "long frame")

let private (|IsChar|_|) b chr=
    if b = chr then Some () else None

let parseAny: Parser<Frame> =
    parser {
        let! start = peekU8
        match start with
        | IsChar Frame.confirmationStartByte -> return! parseConfirmation
        | IsChar Frame.shortFrameStartByte -> return! parseShortFrame |>> ShortFrame
        | IsChar Frame.longFrameStartByte -> return! parseLongFrame |>> LongFrame
        | _ -> return! fail $"invalid start byte: 0x{start:X2}"
    }
