namespace Metering.Mbus.Protocol.Frames.TransportLayer

type NumberOfEncryptedBlocks =
    private NumberOfEncryptedBlocks of int

module NumberOfEncryptedBlocks =

    let value (NumberOfEncryptedBlocks v) =
        v

    let tryCreate v =
        if v <= 0x0E
        then v |> NumberOfEncryptedBlocks |> Some
        else None

    let private mask = 0xF0us
    let private shift = 4

    let map cnf =
        (cnf &&& mask) >>> shift
        |> int
        |> NumberOfEncryptedBlocks
