namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.ParserSource
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
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

type MbusProtocolUserDataRaw =
    | Fragment of Field<FragmentLinkUserData>
    | Complete of Field<CompleteLinkUserDataRaw>

module MbusProtocolUserDataRaw =

    let private isFragment _ = false

    let private parseApl
        (tpl: TplRaw)
        (source: Field<AplDataExpanded>)
        : Parser<AplRaw> =

        let ci = TplRaw.ci tpl

        parser {
            match source.Value with
            | Protected protectedBytes ->
                return
                    source
                    |> Field.withValue protectedBytes
                    |> AplRaw.Protected

            | Unprotected unprotectedBytes ->
                return!
                    parseExactly unprotectedBytes (AplRaw.parse ci.Value)
                    |>> Field.value
                    |>> Parsed
        }

    let parse
        (extCtxResolver: IExternalSecurityContextResolver)
        : Parser<MbusProtocolUserDataRaw> =

        parseField "Link Layer User Data"
        <| parser {
            let! ell = EllRaw.tryParse

            let! nwl = NwlRaw.tryParse

            let! afl = AflRaw.tryParse

            if isFragment afl then
                return! fail "Fragmented messages are not supported"

            let! tpl = TplRaw.parse

            let! expansion = TplRaw.expand extCtxResolver tpl

            let! apl = parseApl tpl.Value expansion

            return
                {
                    Ell = ell
                    Nwl = nwl
                    Afl = afl
                    Tpl = tpl
                    Apl = apl
                }
        }
        |>> Complete

type CompleteLinkUserData =
    {
        Ell: Field<Ell> option
        Nwl: Field<Nwl> option
        Afl: Field<Afl> option
        Tpl: Field<Tpl>
        Apl: Apl
    }
type MbusProtocolUserData =
    | Fragment of Field<FragmentLinkUserData>
    | Complete of Field<CompleteLinkUserData>

module MbusProtocolUserData =

    let private traverseOption f =
        function
        | None -> passed None
        | Some value ->
            f value
            |> map Some

    let fromRaw
        (raw: MbusProtocolUserDataRaw)
        : Validation<MbusProtocolUserData> =

        validator {
            match raw with
            | MbusProtocolUserDataRaw.Fragment fragment ->
                return Fragment fragment

            | MbusProtocolUserDataRaw.Complete complete ->
                let! ell =
                    complete.Value.Ell
                    |> traverseOption (validateField Ell.fromRaw)

                and! nwl =
                    complete.Value.Nwl
                    |> traverseOption (fun field -> failed field "Failed to validate NWL field.")

                and! afl =
                    complete.Value.Afl
                    |> traverseOption (fun field -> failed field "Failed to validate AFL field.")

                and! tpl = validateField Tpl.fromRaw complete.Value.Tpl

                and! apl = Apl.fromRaw complete.Value.Apl

                let data: CompleteLinkUserData = {
                    Ell = ell
                    Nwl = nwl
                    Afl = afl
                    Tpl = tpl
                    Apl = apl
                }

                return
                    Field.withValue data complete
                    |> Complete
        }