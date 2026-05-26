namespace Metering.Dlms.Protocol.CosemApdu

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

type CosemApduRaw =
    | Acse of AcseApduRaw
    | Xdlms of XdlmsApduRaw

module CosemApduRaw =

    let decode : Parser<ParsedField<CosemApduRaw>> =
        parser {
            let! tag = CosemApduTag.peek

            match tag with
            | CosemApduTag.Acse _ ->
                return! AcseApduRaw.parse |>> map CosemApduRaw.Acse

            | CosemApduTag.Xdlms _ ->
                return! XdlmsApduRaw.parse |>> map CosemApduRaw.Xdlms
        }

    let validate (raw: CosemApduRaw) : Result<unit, PError> =
    let decode (raw: CosemApduRaw) : CosemApdu =
