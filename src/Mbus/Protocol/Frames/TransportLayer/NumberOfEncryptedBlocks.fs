namespace Metering.Mbus.Protocol.Frames.TransportLayer

type EncryptedBlockCount =
    private EncryptedBlockCount of int

type EncryptedLengthIndicator =
    | NoEncryptedData
    | FixedEncryptedBlocks of EncryptedBlockCount
    | AllRemainingDataEncrypted

module EncryptedBlockCount =

    let value (EncryptedBlockCount v) =
        v

    let tryCreate v =
        if v >= 1 && v <= 0x0E
        then v |> EncryptedBlockCount |> Some
        else None

module EncryptedLengthIndicator =

    let private mask = 0xF0us
    let private shift = 4

    let map cnf =
        match (cnf &&& mask) >>> shift |> int with
        | 0 ->
            NoEncryptedData

        | 0x0F ->
            AllRemainingDataEncrypted

        | value ->
            value
            |> EncryptedBlockCount
            |> FixedEncryptedBlocks
