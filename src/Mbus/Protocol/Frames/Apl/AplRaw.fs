namespace Metering.Mbus.Protocol.Frames.Apl

type AplRaw =
    | RspUdData of RspUdDataRaw
    | AlarmBits of uint8
    | SelectedDevice of ReadOnlyMemory<uint8>
    | SndUdData of CmdRecordRaw list