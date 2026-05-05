module Mbus.Frames.TplParser

open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core
open Mbus.Frames
open Mbus.Frames.StatusField
open Mbus.Frames.Tpl

let private (|IsFunc|_|) tag b=
    if b = fst tag then Some () else None

let private ciOnlyCodes = [| fst ciAplSelect; fst ciCommand; fst ciDevSelect |]
let private shortCodes  = [| fst ciRspShort |]
let private longCodes   = [| fst ciRspLong; fst ciAlarm |]

let private mem xs b = System.Array.Exists(xs, fun x -> x = b)

let private (|CiOnlyKind|ShortKind|LongKind|UnknownCi|) (b: byte) =
    if mem ciOnlyCodes b then CiOnlyKind
    elif mem shortCodes b then ShortKind
    elif mem longCodes b then LongKind
    else UnknownCi b

let parseCiOnly : Parser<TplCiOnlyFunc> =
   parser {
       let! ci = parseU8
       match ci with
       | IsFunc ciAplSelect -> return AplSelect
       | IsFunc ciCommand -> return Command
       | IsFunc ciDevSelect -> return DevSelect
       | _ -> return! failBefore $"unexpected CI for TPL None: 0x{ci:X2}"
    }

let private parseCiForShort : Parser<TplShortFunc> =
    parser {
        let! ci = parseU8
        match ci with
        | IsFunc ciRspShort -> return TplShortFunc.Rsp
        | _ -> return! failBefore $"unexpected CI for TPL Short: 0x{ci:X2}"
    }

let parseShort : Parser<TplShort> =
    parser {
        let! ci = parseCiForShort
        let! acc = parseU8
        let! status = parseStatusField
        let! cnf = parseU16LittleEndian
        return { Func = ci; Acc = acc; Status = status; Cnf = cnf }
    }

let private parseCiForLong : Parser<TplLongFunc> =
    parser {
        let! ci = parseU8
        match ci with
        | IsFunc ciRspLong -> return TplLongFunc.Rsp
        | IsFunc ciAlarm -> return TplLongFunc.Alarm
        | _ -> return! failBefore $"unexpected CI for TPL Long: 0x{ci:X2}"
    }

let parseLong : Parser<TplLong> =
    parser {
        let! ci = parseCiForLong
        let! ala = AddressParser.parseAla
        let! acc = parseU8
        let! status = parseStatusField
        let! cnf = parseU16LittleEndian
        return { Func = ci; Ala = ala; Acc = acc; Status = status; Cnf = cnf }
    }

let parseAny: Parser<Tpl> =
    parser {
        let! b = peekU8
        match b with
        | CiOnlyKind -> return! parseCiOnly |>> CiOnly
        | ShortKind -> return! parseShort |>> Short
        | LongKind -> return! parseLong |>> Long
        | UnknownCi b -> return! fail $"unexpected CI for TPL: 0x{b:X2}"
    }