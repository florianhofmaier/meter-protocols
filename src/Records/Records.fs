namespace Mbus.Records

open System
open Mbus
open Mbus.Records.DataInfoBlocks
open Mbus.Records.ValueInfoBlocks

type RspDataRecord = {
    Value: MbusValue
    StNum: StorageNumber
    Fn: MbusFunctionField
    Tariff: Tariff
    SubUnit: SubUnit
    Vib: RspVib
}

type CmdRecord = {
    Value: MbusValue
    StNum: StorageNumber
    Fn: MbusFunctionField
    Tariff: Tariff
    SubUnit: SubUnit
    Vib: CmdVib
}

type SpecialFunction =
    | IdleFiller
    | MfrData of ReadOnlyMemory<byte>
    | MfrDataMoreFollows of ReadOnlyMemory<byte>

type RspRecord =
    | Data of RspDataRecord
    | SpecialFunction of SpecialFunction

module Record =
    module Parser =
        open Mbus.BaseParsers.BinaryParsers
        open Mbus.BaseParsers.Core

        let private maskSpecFn = 0x70uy
        let private shiftSpecFn = 4
        let private mfrData = 0x00uy
        let private mfrDataMoreFollows = 0x01uy
        let private idleFiller = 0x02uy
        let private globReadReq = 0x07uy
        let private isSpecFn b = (b &&& 0x0Fuy) = 0x0Fuy

        let private (|IsSpecFn|_|) expected b =
            if (b &&& maskSpecFn) >>> shiftSpecFn = expected then Some () else None

        let parseSpecFn b : Parser<SpecialFunction> = parser {
            match b with
            | IsSpecFn idleFiller -> return IdleFiller
            | IsSpecFn mfrData ->
                let! data = takeAllMem
                return MfrData data
            | IsSpecFn mfrDataMoreFollows ->
                let! data = takeAllMem
                return MfrDataMoreFollows data
            | IsSpecFn globReadReq -> return! failBefore "global readout request not supported"
            | _ -> return! failBefore $"invalid special function code: 0x{b:X2}"
        }

        let parseDataRec dif : Parser<RspDataRecord> = parser {
            let! fn, stNum, tariff, subUnit = DibParser.parse dif
            let! vib = Vib.Parser.parseRsp
            let! value = MbusValue.parse dif
            return { Fn = fn; StNum = stNum; Tariff = tariff; SubUnit = subUnit; Vib = vib; Value = value; }
        }

        let parseRspRec: Parser<RspRecord> = parser {
            let! dif = parseU8
            if isSpecFn dif then
                let! record = parseSpecFn dif |> withCtx "special function record"
                return SpecialFunction record
            else
                let! record = parseDataRec dif |> withCtx "data record"
                return Data record
        }

        let parseCmdRec dif : Parser<CmdRecord> = parser {
            let! fn, stNum, tariff, subUnit = DibParser.parse dif
            let! vib = Vib.Parser.parseCmd
            let! value = MbusValue.parse dif
            return { Fn = fn; StNum = stNum; Tariff = tariff; SubUnit = subUnit; Vib = vib; Value = value; }
        }

        let parseCmdRecord : Parser<CmdRecord> = parser {
            let! dif = parseU8
            return! parseCmdRec dif |> withCtx "cmd data record"
        }

    module Writer =
        open Mbus.BaseWriters.Core
        open Mbus.Records

        let write (record: RspDataRecord) : Writer<unit> =
            writer {
                do! DibWriter.writeDib record.Fn record.StNum record.Tariff record.SubUnit record.Value
                match record.Vib with
                | RspVib.Normal nv -> do! Vib.Writer.writeNormalVib nv
                | vib -> return! writerError $"Unsupported Vib type for writing: {vib}"
                do! MbusValue.write record.Value
            }

        let writeCmd (record: CmdRecord) : Writer<unit> =
            writer {
                do! DibWriter.writeDib record.Fn record.StNum record.Tariff record.SubUnit record.Value
                match record.Vib with
                | CmdVib.Normal nv -> do! Vib.Writer.writeCmdVib nv
                | vib -> return! writerError $"Unsupported CmdVib type for writing: {vib}"
                do! MbusValue.write record.Value
            }
