namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.TransportLayer

type UnfragmentedLinkUserDataRaw =
    {
        Tpl: Field<TplRaw>
    }


type LinkUserDataRaw =
    | Unfragmented of UnfragmentedLinkUserDataRaw
