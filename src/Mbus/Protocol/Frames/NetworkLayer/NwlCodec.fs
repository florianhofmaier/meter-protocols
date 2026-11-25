namespace Metering.Mbus.Protocol.Frames.NetworkLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

/// No supported NWL variants exist yet; callers cannot construct a successful value.
[<Sealed>]
type Nwl private () = class end

module NwlCodec =

    let tryParse: Parser<Field<Nwl> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | CiField.Nwl ->
                return! fail $"Layer introduced by CI 0x{CiField.code ci:X2} is not implemented."
            | _ -> return None
        }

    let validate (raw: Field<Nwl>) : Validation<Field<Nwl>> = failed raw "NWL is not implemented."

    let tryDecode
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<Nwl> option * Field<ReadOnlyMemory<byte>>> =

        decoder {
            let! raw =
                Decoder.parseValue
                    (parser {
                        let! raw = tryParse
                        let! _ = takeAll
                        return raw
                    })
                    source

            match raw with
            | None -> return None, source
            | Some raw ->
                let! valid = Decoder.validate validate raw
                return Some valid, source
        }
