module Mbus.Frames.FrameWriter

open Mbus.BaseWriters.BinaryWriters
open Mbus.BaseWriters.Core

let writeConfirmation: Writer<unit> =
    writer {
        do! writeU8 Frame.confirmationStartByte
    }

let private writeCrc start len : Writer<unit> =
    writer {
        let! crcMem = getMem start len
        let crc = Frame.calcCrc crcMem
        do! writeU8 crc
    }

let writeShortFrame (shortFrame: ShortFrame) : Writer<unit> =
    writer {
        do! writeU8 Frame.shortFrameStartByte
        do! writeU8 shortFrame.CField
        do! writeU8 shortFrame.PrmAdr
        do! writeCrc 1 2
        do! writeU8 Frame.stopByte
    }

type LongFramePositions private = {
    StartUdPos: int
    LenUdPos: int
}

let private writeLongFrameHeader : Writer<LongFramePositions> =
    writer {
        do! writeU8 Frame.longFrameStartByte
        let! lenUdPos = getPos
        do! writeU16 0us // placeholder for length bytes
        do! writeU8 Frame.longFrameStartByte
        let! startUd = getPos
        return { StartUdPos = startUd; LenUdPos = lenUdPos }
    }

let private backTrackLongFrameLFields positions : Writer<int> =
    writer {
        let! currentPos = getPos
        let lenUd = byte (currentPos - positions.StartUdPos)
        do! seek positions.LenUdPos
        do! writeU8 lenUd
        do! writeU8 lenUd
        do! seek currentPos
        return! getPos
    }

let writeLongFrame longFrame =
    writer {
        let! positions = writeLongFrameHeader
        do! writeU8 longFrame.CField
        do! writeU8 longFrame.PrmAdr
        do! TplWriter.write longFrame.Tpl
        do! AplWriter.write longFrame.Apl
        let! currentPos = backTrackLongFrameLFields positions
        do! writeCrc positions.StartUdPos (currentPos - positions.StartUdPos)
        do! writeU8 Frame.stopByte
    }

let writeFrame frame =
    match frame with
    | Confirmation-> writeConfirmation
    | ShortFrame sf -> writeShortFrame sf
    | LongFrame lf -> writeLongFrame lf
