namespace Metering.Mbus.Protocol.Frames.Transport

type NumberOfEncryptedBlocks =
    private NumberOfEncryptedBlocks of int

module NumberOfEncryptedBlocks =

    let private mask = 0xF0us
    let private shift = 4

    let value (NumberOfEncryptedBlocks v) =
        v

    let tryCreate v =
        if v >= 0 && v < 0x0F
        then v |> NumberOfEncryptedBlocks |> Some
        else None

    let tryMap cnf =
        (cnf &&& mask) >>> shift
        |> int
        |> tryCreate

