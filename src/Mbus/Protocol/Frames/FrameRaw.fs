namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.Apl
open Metering.Mbus.Protocol.Frames.Tpl

type FixedLengthRaw =
    {
        CField: ParsedField<uint8>
        PrmAdr: ParsedField<uint8>
    }

type VariableLengthRaw =
    {
        CField: ParsedField<uint8>
        PrmAdr: ParsedField<uint8>
        Tpl: TplRaw
        Apl: ParsedField<AplRaw>
    }

type FrameRaw =
    | Confirmation of byte
    | FixedLength of FixedLengthRaw
    | VariableLength of VariableLengthRaw
