namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility

module EllCodec =

    let parse = EllRaw.parse

    let tryParse = EllRaw.tryParse

    let validate (raw: Field<EllRaw>) : Validation<Field<Ell>> = validateField Ell.validate raw

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Ell>> =
        decoder {
            let! raw = Decoder.parse parse source
            return! Decoder.validate validate raw
        }
