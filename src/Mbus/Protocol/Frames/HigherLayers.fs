namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

/// Structural results. NWL/AFL have only absent results in the supported scope.
type HigherLayersRaw = {
    Ell: Field<EllRaw> option
    Nwl: Field<Nwl> option
    Afl: Field<Afl> option
    Tpl: Field<TplRaw>
}

/// The collectively validated header block, before protection input preparation.
type HigherLayersHeaders = {
    Ell: Field<Ell> option
    Nwl: Field<Nwl> option
    Afl: Field<Afl> option
    Tpl: Field<Tpl>
}

/// Validated layers and unprocessed content with frame-derived protection inputs.
type HigherLayers = {
    Ell: Field<Ell> option
    Nwl: Field<Nwl> option
    Afl: Field<Afl> option
    Tpl: Field<Tpl>
    Protection: MbusProtectionInput
}
