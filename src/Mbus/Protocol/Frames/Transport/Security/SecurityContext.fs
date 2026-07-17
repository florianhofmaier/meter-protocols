namespace Metering.Mbus.Protocol.Security

open System

type Mode5SecurityContext =
    private Key of ReadOnlyMemory<uint8>

type SecurityContext =
    | NoSecurity
    | Mode5 of Mode5SecurityContext

module Mode5SecurityContext =

    let value (Key v) = v

module SecurityContext =

    let none =
        NoSecurity

    let mode5 key =
        Mode5 (Key key)

