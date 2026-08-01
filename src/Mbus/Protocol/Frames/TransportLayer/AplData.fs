namespace Metering.Mbus.Protocol.Frames.TransportLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.ApplicationLayer

type AplDataRaw =
    private AplData of ReadOnlyMemory<byte>

module AplDataRaw =

    let bytes (AplData bytes) =
        bytes

    let create bytes =
        AplData bytes

    let toParserSource
        (field: Field<AplDataRaw>)
        : Field<ReadOnlyMemory<byte>> =

        field |> Field.map bytes

type AplDataExpanded =
    | Unprotected of Field<ReadOnlyMemory<byte>>
    | Protected of AplProtectedRaw