namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility

module EllCodec =

    let parse = EllRaw.parse

    let tryParse = EllRaw.tryParse

    let fromRaw (raw: Field<EllRaw>) : Validation<Field<Ell>> =
        validateField Ell.fromRaw raw

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Ell>> =
        decoder {
            let! raw = Core.parse parse source
            return! Core.validate fromRaw raw
        }

    let tryDecode
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<Ell> option * Field<ReadOnlyMemory<byte>>> =

        decoder {
            let! raw =
                Core.parseValue
                    (parser {
                        let! raw = tryParse
                        // An absent layer leaves the supplied byte field unchanged.
                        // Only this temporary reader must satisfy the exact boundary.
                        let! _ = takeAll
                        return raw
                    })
                    source

            match raw with
            | None -> return None, source
            | Some raw ->
                let! field = Core.validate fromRaw raw
                let (Ell.NoDllEncryption header) = field.Value
                return Some field, header.Payload
        }
