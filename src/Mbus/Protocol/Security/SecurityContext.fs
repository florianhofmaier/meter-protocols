namespace Metering.Mbus.Protocol.Security

open System
open Metering.Mbus.Protocol.Frames.DeviceIdentification

type MeterAddress =
    DeviceIdentification

type Mode5SecurityContext =
    {
        Key : ReadOnlyMemory<byte>
        MeterAddress : MeterAddress option
    }

type SecurityContext =
    | NoSecurity
    | Mode5 of Mode5SecurityContext

module SecurityContext =

    let none =
        NoSecurity

    let mode5 key meterAddress =
        Mode5
            {
                Key = key
                MeterAddress = meterAddress
            }
