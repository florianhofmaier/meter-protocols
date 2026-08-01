namespace Metering.Mbus.Protocol.Frames.TransportLayer

type Mode =
    | Mode0
    | Mode5

module Mode =

    let mask = 0x1F00us
    let shift = 8

    let rawValue configurationField =
        (configurationField &&& mask) >>> shift

    let tryMap cnf =
        match rawValue cnf with
        | 0x00us -> Some Mode0
        | 0x05us -> Some Mode5
        | _ -> None
