namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Security.ProtectedApdus
open Metering.Dlms.Protocol.Xdlms

type GloInitiateRequestRaw =
    private ProtectedApduRaw of Field<ProtectedApduRaw>

module GloInitiateRequestRaw =

    let value (ProtectedApduRaw protectedApdu) =
        protectedApdu

    let parse : Parser<Field<GloInitiateRequestRaw>> =
        parseField "glo-initiate-request"
        <| parser {
            do!
                Tag.expect XdlmsTag.GloInitiateRequest

            let! protectedApdu =
                Axdr.OctetString.parseContent ProtectedApduRaw.parse

            return
                ProtectedApduRaw protectedApdu
        }