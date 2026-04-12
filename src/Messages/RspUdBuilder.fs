namespace Mbus.Messages

open Mbus
open Mbus.Frames
open Mbus.Records

type RspUdBuilder =
    private { CField: uint8
              PrmAdr: uint8
              Tpl: TplLong
              Records: RspDataRecord list }

module RspUdBuilder =

    let init ala =
        { CField = CField.rspUd
          PrmAdr = 0uy
          Tpl = { Func = TplLongFunc.Rsp; Ala = ala; Acc = 0uy; Status = MbusStatusField.CreateEmpty; Cnf = 0us }
          Records = [] }

    let withDfcSet builder : RspUdBuilder =
        { builder with CField = CField.setDfc builder.CField }

    let withAcdSet builder : RspUdBuilder =
        { builder with CField = CField.setAcd builder.CField }

    let withPrimaryAddress addr builder : RspUdBuilder =
        { builder with PrmAdr = addr }

    let addDataRecord record builder : RspUdBuilder =
        { builder with Records = builder.Records @ [ record ] }

    let withStatus status builder : RspUdBuilder =
        { builder with Tpl.Status = status }

    let withConfigField cnf builder : RspUdBuilder =
        { builder with Tpl.Cnf = cnf }

    let withAccessNumber acc builder : RspUdBuilder =
        { builder with Tpl.Acc = acc }

    let build builder =
        LongFrame {
            CField = builder.CField
            PrmAdr = builder.PrmAdr
            Tpl = builder.Tpl |> Tpl.Long
            Apl = { DataRecords = builder.Records; MfrSpecificData = None; IsMoreDataInNextTelegram = false } |> RspUdData
        }
