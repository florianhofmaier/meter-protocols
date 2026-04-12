namespace Mbus.Frames

open System
open Mbus.Records

module DeviceSelection =
    let length = 8

type RspUdData =
    { DataRecords: RspDataRecord list
      MfrSpecificData: ReadOnlyMemory<byte> option
      IsMoreDataInNextTelegram: bool }

type Apl =
    | RspUdData of RspUdData
    | AlarmBits of uint8
    | SelectedDevice of byte[]
    | SndUdData of CmdRecord list