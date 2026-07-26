namespace Metering.Mbus.Protocol.Frames.TransportLayer

type Mode =
    | Mode0
    | Mode5

type SecurityModeClassification =
    | SupportedMode0
    | SupportedMode5
    | StandardDefinedUnsupported of byte
    | Reserved of byte

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

    /// EN 13757-7:2018, 7.5.8, Table 19 (printed page 33).
    let classify =
        function
        | 0uy -> SupportedMode0
        | 5uy -> SupportedMode5
        | (1uy | 2uy | 3uy | 4uy | 7uy | 8uy | 9uy | 10uy
          | 13uy | 15uy) as mode ->
            StandardDefinedUnsupported mode
        | mode ->
            Reserved mode

    let unsupportedMessage mode =
        match classify mode with
        | StandardDefinedUnsupported value ->
            $"Unsupported, but standard-conformant security mode {value}. EN 13757-7:2018, 7.5.8, Table 19."

        | Reserved value ->
            $"Reserved/standard-invalid security mode value {value}. EN 13757-7:2018, 7.5.8, Table 19."

        | SupportedMode0
        | SupportedMode5 ->
            invalidArg
                (nameof mode)
                $"Security mode {mode} is supported and has no unsupported-mode diagnostic."
