module Mbus.Frames.AplParser

open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core
open Mbus.Frames
open Mbus.Records

let parseRspRecords l : Parser<RspRecord list> = parser {
    let p = parseUntilEnd Record.Parser.parseRspRec
    return! runOnSubSlice l p
}

let parseCmdRecords l : Parser<CmdRecord list> = parser {
    let p = parseUntilEnd Record.Parser.parseCmdRecord
    return! runOnSubSlice l p
}

let private extractDataRecords records =
    records |> List.choose (function Data dr -> Some dr | _ -> None)

let private extractMfrData records =
    records |> List.tryPick (function
        | SpecialFunction (MfrData d) -> Some (d, false)
        | SpecialFunction (MfrDataMoreFollows d) -> Some (d, true)
        | _ -> None)

let parseRspUdData l : Parser<RspUdData> = parser {
    let! records = parseRspRecords l
    let mfrResult = extractMfrData records
    return { DataRecords = extractDataRecords records
             MfrSpecificData = mfrResult |> Option.map fst
             IsMoreDataInNextTelegram = mfrResult |> Option.map snd |> Option.defaultValue false }
}

let parseAlarm : Parser<uint8> = parser {
    return! parseU8
}

let parseDeviceSelection l : Parser<byte[]> =
    parser {
        if l <> DeviceSelection.length then
            return! fail $"invalid length for device selection: expect 8, got {l}"
        else
            let! mem = takeMem DeviceSelection.length
            return mem.ToArray()
    }

let parseAny l tpl: Parser<Apl> =
    (parser {
        match tpl with
        | Long { Func = TplLongFunc.Rsp } ->
            let! apl = parseRspUdData l |> withCtx "records"
            return RspUdData apl
        | Long { Func = TplLongFunc.Alarm } ->
            let! apl = parseAlarm |> withCtx "alarms"
            return AlarmBits apl
        | Short { Func = TplShortFunc.Rsp } ->
            let! apl = parseRspUdData l |> withCtx "records"
            return RspUdData apl
        | CiOnly DevSelect ->
            let! apl = parseDeviceSelection l |> withCtx "secondary selection"
            return SelectedDevice apl
        | CiOnly Command ->
            let! apl = parseCmdRecords l |> withCtx "records"
            return SndUdData apl
        | CiOnly AplSelect ->
            return! fail "ci apl select not supported"
    }) |> withCtx "application layer"
