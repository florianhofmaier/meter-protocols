namespace Metering.Mbus.Protocol.Frames.TransportLayer

type TplHeaderRaw =
    | Short of ShortHeaderRaw
    | Long of LongHeaderRaw