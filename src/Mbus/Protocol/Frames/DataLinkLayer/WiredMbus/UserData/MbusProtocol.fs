namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.ParserSource
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type FragmentData =
    private FragmentData of ReadOnlyMemory<byte>

module FragmentData =
    let value (FragmentData value) = value

type FragmentLinkUserData =
    {
        Ell: Field<EllRaw> option
        Afl: Field<AflRaw>
        Data: Field<FragmentData>
    }

type CompleteLinkUserDataRaw =
    {
        Ell: Field<EllRaw> option
        Nwl: Field<NwlRaw> option
        Afl: Field<AflRaw> option
        Tpl: Field<TplRaw>
        Apl: AplRaw
    }

type MbusProtocolRaw =
    | Fragment of FragmentLinkUserData
    | Complete of CompleteLinkUserDataRaw

module MbusProtocolRaw =

    let private isFragment _ = false

    let private parseApl
        (tpl: TplRaw)
        (source: Field<AplDataExpanded>)
        : Parser<AplRaw> =

        let ci = TplRaw.ci tpl

        parser {
            match source.Value with
            | Protected protectedBytes ->
                return AplRaw.Protected protectedBytes

            | Unprotected unprotectedBytes ->
                return!
                    parseExactly unprotectedBytes (AplRaw.parse ci.Value)
                    |>> Field.value
                    |>> Parsed
        }

    let parse
        (extCtxResolver: IExternalSecurityContextResolver)
        : Parser<MbusProtocolRaw> =

        parser {
            let! ell = EllRaw.tryParse
            let! nwl = NwlRaw.tryParse
            let! afl = AflRaw.tryParse

            if isFragment afl then
                return! fail "Fragmented messages are not supported"

            let! tpl = TplRaw.parse

            let! expansion = TplRaw.expand extCtxResolver tpl

            let! apl = parseApl tpl.Value expansion

            return
                Complete {
                    Ell = ell
                    Nwl = nwl
                    Afl = afl
                    Tpl = tpl
                    Apl = apl
                }
        }
