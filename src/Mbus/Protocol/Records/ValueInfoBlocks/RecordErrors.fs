namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records

type RecordError =
    | NoError
    | TooManyDifes
    | StorageNumberNotImplemented
    | UnitNumberNotImplemented
    | TariffNumberNotImplemented
    | FunctionNotImplemented
    | DataClassNotImplemented
    | DataSizeNotImplemented
    | TooManyVifes
    | IllegalVifGroup
    | IllegalVifExponent
    | VifDifMismatch
    | UnimplemetedAction
    | NoDataAvailable
    | DataOverflow
    | DataUnderflow
    | DataError
    | PrematureEndOfRecord

module RecordError =

    let private table =
        Map [
            0x00uy, NoError
            0x01uy, TooManyDifes
            0x02uy, StorageNumberNotImplemented
            0x03uy, UnitNumberNotImplemented
            0x04uy, TariffNumberNotImplemented
            0x05uy, FunctionNotImplemented
            0x06uy, DataClassNotImplemented
            0x07uy, DataSizeNotImplemented
            0x0Buy, TooManyVifes
            0x0Cuy, IllegalVifGroup
            0x0Duy, IllegalVifExponent
            0x0Euy, VifDifMismatch
            0x0Fuy, UnimplemetedAction
            0x15uy, NoDataAvailable
            0x16uy, DataOverflow
            0x17uy, DataUnderflow
            0x18uy, DataError
            0x1Cuy, PrematureEndOfRecord
        ]

    let tryMap
        (bytes: ReadOnlyMemory<byte>)
        (pos: int)
        : RecordError option * int =

        Utility.tryMap table bytes pos

    let map
        (infoBlock: ParsedField<InfoBlockRaw>)
        (pos: int)
        : Validation<RecordError> * int =

        let bytes = InfoBlockRaw.bytes infoBlock.Value
        let error, nextPos = tryMap bytes pos

        match error with
        | Some value -> passed value, nextPos
        | _ ->
            if pos < bytes.Length then
                failed infoBlock $"Unknown record error code 0x{bytes.Span[pos]:X2} at index {pos}", pos
            else
                failed infoBlock $"Missing record error code at index {pos}", pos
