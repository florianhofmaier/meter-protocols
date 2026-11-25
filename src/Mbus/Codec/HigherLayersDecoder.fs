namespace Metering.Mbus.Codec

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

module CoreDecoder = Metering.Common.Decoding.Decoders.Core.Decoder

module HigherLayersDecoder =

    let parse: Parser<HigherLayersRaw> =
        parser {
            let! ell = EllCodec.tryParse
            let! nwl = NwlCodec.tryParse
            let! afl = AflCodec.tryParse
            let! tpl = TplCodec.parse

            return {
                Ell = ell
                Nwl = nwl
                Afl = afl
                Tpl = tpl
            }
        }

    let private validateOptional validate =
        function
        | None -> passed None
        | Some raw -> validate raw |> map Some

    let validate (raw: Field<HigherLayersRaw>) : Validation<Field<HigherLayersHeaders>> =
        validator {
            let! ell = validateOptional EllCodec.validate raw.Value.Ell
            and! nwl = validateOptional NwlCodec.validate raw.Value.Nwl
            and! afl = validateOptional AflCodec.validate raw.Value.Afl
            and! tpl = TplCodec.validate raw.Value.Tpl

            let headers: HigherLayersHeaders = {
                Ell = ell
                Nwl = nwl
                Afl = afl
                Tpl = tpl
            }

            return Field.withValue headers raw
        }

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<HigherLayers>> =

        decoder {
            let! parsed = CoreDecoder.parseValue parse source
            let! headers = CoreDecoder.validate validate (Field.withValue parsed source)
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
