namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.UserData

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer

type FragmentData =
    private FragmentData of ReadOnlyMemory<byte>

module FragmentData =

    let value (FragmentData value) =
        value

type FragmentLinkUserData =
    {
        Ell: Field<EllRaw> option
        Afl: Field<TplRaw>
        Data: Field<FragmentData>
    }

type CompleteLinkUserDataRaw =
    {
        Ell: Field<EllRaw> option
        Nwl: Field<NwlRaw> option
        Afl: Field<AflRaw> option
        Tpl: Field<TplRaw>
    }

type MbusProtocol =
    | Fragment of FragmentLinkUserData
    | Complete of CompleteLinkUserDataRaw

module MbusProtocol =

    let private isFragment _ =
        false

    let parse : Parser<MbusProtocol> =
        parser {
            let! ell = EllRaw.tryParse

            let! nwl = NwlRaw.tryParse

            let! afl = AflRaw.tryParse

            if isFragment afl then
                return!
                    fail
                    "Fragmented messages are not supported"

            let! tpl = TplRaw.parse

            return
                Complete {
                    Ell = ell
                    Nwl = nwl
                    Afl = afl
                    Tpl = tpl
                }
        }