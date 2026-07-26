namespace Metering.Mbus.Protocol.Frames.TransportLayer

type ContentOfMessage =
    | StandardData
    | StaticMessage

module ContentOfMessage =

    let mask = 0x0Cus
    let shift = 2

    let tryMap cnf =
        match (cnf &&& mask) >>> shift with
        | 0x00us -> Some StandardData
        | 0x02us -> Some StaticMessage
        | _ -> None