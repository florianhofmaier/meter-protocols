namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System

type Mode5Key =
    private Mode5Key of ReadOnlyMemory<byte>

type Mode5KeyCreationError =
    | InvalidLength of actualLength: int

type Mode5SecurityContext =
    private Mode5SecurityContext of Mode5Key

type SecurityContext =
    | NoSecurity
    | Mode5 of Mode5SecurityContext

module Mode5Key =

    let create
        (bytes: ReadOnlyMemory<byte>)
        : Result<Mode5Key, Mode5KeyCreationError> =

        if bytes.Length = 16
        then Ok (Mode5Key bytes)
        else Error (InvalidLength bytes.Length)

    let value
        (Mode5Key bytes)
        : ReadOnlyMemory<byte> =

        bytes

module Mode5SecurityContext =

    let create
        (key: Mode5Key)
        : Mode5SecurityContext =

        Mode5SecurityContext key

    let key
        (Mode5SecurityContext key)
        : Mode5Key =

        key

    let keyBytes context =
        context
        |> key
        |> Mode5Key.value

module SecurityContext =

    let none =
        NoSecurity

    let mode5 key =
        key
        |> Mode5SecurityContext.create
        |> Mode5
