namespace Metering.Mbus.Protocol

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ParserSource
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

/// Orchestrates the supported higher layers from bounded Link User Data.
/// AFL is rejected until fragment processing and reassembly are implemented.
module HigherLayersDecoder =

    let private parseFollowing =
        parser {
            let! nwl = NwlCodec.tryParse
            let! afl = AflCodec.tryParse
            let! tpl = TplCodec.parse
            return nwl, afl, tpl
        }

    let parse : Parser<HigherLayersRaw> =
        parser {
            let! ell = EllCodec.tryParse
            let! nwl, afl, tpl =
                match ell with
                | None -> parseFollowing
                | Some { Value = EllRaw.NoDllEncryption header } ->
                    parseExactly header.Payload parseFollowing
            return { Ell = ell; Nwl = nwl; Afl = afl; Tpl = tpl }
        }

    let private validateOptional validate =
        function
        | None -> passed None
        | Some raw -> validate raw |> map Some

    let fromRaw (raw: Field<HigherLayersRaw>) : Validation<Field<HigherLayersHeaders>> =
        validator {
            let! ell = validateOptional EllCodec.fromRaw raw.Value.Ell
            and! nwl = validateOptional NwlCodec.fromRaw raw.Value.Nwl
            and! afl = validateOptional AflCodec.fromRaw raw.Value.Afl
            and! tpl = TplCodec.fromRaw raw.Value.Tpl
            let headers: HigherLayersHeaders = { Ell = ell; Nwl = nwl; Afl = afl; Tpl = tpl }
            return Field.withValue headers raw
        }

    let decode
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<HigherLayers>> =

        decoder {
            let! parsed = Core.parseValue parse source
            let! headers = Core.validate fromRaw (Field.withValue parsed source)
            let! protection = MbusProtectionInput.decode headers.Value.Tpl.Value
            let higher = {
                Ell = headers.Value.Ell
                Nwl = headers.Value.Nwl
                Afl = headers.Value.Afl
                Tpl = headers.Value.Tpl
                Protection = protection
            }
            return Field.withValue higher headers
        }
