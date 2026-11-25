namespace Metering.Mbus.Protocol.Messages

open Metering.Common.BaseWriters.BinaryWriters
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

type PrimaryAddress =
    | Unconfigured
    | PrimAdr of PrimAdr

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrimaryAddress =
    /// Writes the unsigned, one-byte link-layer address (EN 13757-3:2025, Table 10, data type C).
    let write =
        function
        | PrimaryAddress.Unconfigured -> writeU8 0uy
        | PrimaryAddress.PrimAdr address -> writeU8 (PrimAdr.value address)
