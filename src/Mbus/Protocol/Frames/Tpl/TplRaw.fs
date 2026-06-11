namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers

type TplShortRaw =
    {
        Acc: ParsedField<uint8>
        Status: ParsedField<uint8>
        Cnf: ParsedField<uint16>
    }

type TplLongRaw =
    {
        IdNum: ParsedField<uint32>
        Mfr: ParsedField<uint16>
        Version: ParsedField<uint8>
        DevType: ParsedField<uint8>
        Acc: ParsedField<uint8>
        Status: ParsedField<uint8>
        Cnf: ParsedField<uint16>
    }

type TplRaw =
    | CiOnly of ParsedField<uint8>
    | Short of ParsedField<TplShortRaw>
    | Long of ParsedField<TplLongRaw>

