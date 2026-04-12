module Mbus.Frames.AplWriter

open Mbus.BaseWriters.BinaryWriters
open Mbus.BaseWriters.Core
open Mbus.Frames
open Mbus.Records

let writeUserData (records: RspRecord list) : Writer<unit> =
    writer {
        for r in records do
            match r with
            | RspRecord.Data dr -> do! Record.Writer.write dr
            | _ -> failwith "Not implemented"
    }

let writeAlarmBits alarm: Writer<unit> =
    writer {
        do! writeU8 alarm
    }

let writeDeviceSelection (select: byte[]) : Writer<unit> =
    writer {
        for b in select do
            do! writeU8 b
    }

let writeCmdData (records: CmdRecord list) : Writer<unit> =
    writer {
        for r in records do
            do! Record.Writer.writeCmd r
    }

let write (apl: Apl) : Writer<unit> =
    writer {
        match apl with
        | Apl.RspUdData rspUd ->
            for dr in rspUd.DataRecords do
                do! Record.Writer.write dr
        | Apl.SndUdData records -> do! writeCmdData records
        | Apl.AlarmBits alarmBits -> do! writeAlarmBits alarmBits
        | Apl.SelectedDevice selection -> do! writeDeviceSelection selection
    }