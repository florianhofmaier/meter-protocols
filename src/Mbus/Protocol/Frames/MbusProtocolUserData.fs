namespace Metering.Mbus.Protocol.Frames

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer

/// The independently decoded layers assembled by the protocol orchestrator.
type CompleteLinkUserData =
    {
        Ell: Field<Ell> option
        Nwl: Field<Nwl> option
        Afl: Field<Afl> option
        Tpl: Field<Tpl>
        Apl: Field<Apl>
    }

type MbusProtocolUserData =
    | Complete of Field<CompleteLinkUserData>
