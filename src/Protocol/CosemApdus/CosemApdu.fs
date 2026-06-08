namespace Metering.Dlms.Protocol.CosemApdu

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse
open Metering.Dlms.Protocol.Xdlms

type CosemApduTag =
    | Acse of AcseTag
    | Xdlms of XdlmsTag

module CosemApduTag =

    let peek =
        parser {
            let! acse = Tag.tryPeek<AcseTag>

            match acse with
            | Some acse ->
                return Acse acse

            | None ->
                let! xdlms = Tag.tryPeek<XdlmsTag>

                match xdlms with
                | Some xdlms ->
                    return Xdlms xdlms

                | None ->
                    return! fail "unknown APDU tag"
        }

type CosemApdu =
    | Acse of AcseApdu
    | Xdlms of XdlmsApdu

module CosemApdu =

    let decode
        (bytes: ParsedField<ReadOnlyMemory<byte>>)
        : Decoder<CosemApdu> =

        decoder {
            let! tag = parseValue CosemApduTag.peek bytes

            match tag with
            | CosemApduTag.Acse _ ->
                return! AcseApdu.decode

            | CosemApduTag.Xdlms _ ->
                return! XdlmsApdu.decode
        }