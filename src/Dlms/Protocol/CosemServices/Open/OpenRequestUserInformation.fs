namespace DlmsMessages.CosemServices.Open

open DlmsMessages.Utility
open DlmsMessages.Xdlms
open Mbus.BaseParsers.Core

type OpenRequestUserInformation =
    | InitiateRequest of BufferSlice
    | GloInitiateRequest of BufferSlice
    | DedInitiateRequest of BufferSlice

module OpenRequestUserInformation =
    let parse len : Parser<OpenRequestUserInformation> =
        withCtx "user-information" <|
        parser {
            let! tag = Tag.parse<XdlmsTag>

            match tag with
            | XdlmsTag.InitiateRequest
            | XdlmsTag.GloInitiateRequest
            | XdlmsTag.DedInitiateRequest ->
                return! BufferSlice.parse len |>> InitiateRequest

            | other ->
                return! fail $"unexpected tag 0x{other:X2} in OPEN.request user-information"
        }
