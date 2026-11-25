namespace Metering.Mbus.Protocol.Frames.TransportLayer

type NumberOfEncryptedBlocks = private NumberOfEncryptedBlocks of int

module NumberOfEncryptedBlocks =

    let value (NumberOfEncryptedBlocks v) = v

    /// Creates the four-bit block count, including 15 for all remaining data.
    let tryCreate v =
        if v >= 0 && v <= 0x0F then
            v |> NumberOfEncryptedBlocks |> Some
        else
            None

    let private mask = 0xF0us
    let private shift = 4

    let map cnf =
        (cnf &&& mask) >>> shift |> int |> NumberOfEncryptedBlocks
