namespace DlmsMessages.Acse

open DlmsMessages
open Mbus.BaseParsers.Core

type AuthenticationValueRaw =
    | CharString of Ber.GraphicString
    | BitString of Ber.BitString

module AuthenticationValueRaw =
    type Tag =
        | CharString = 0x80uy
        | BitString = 0x81uy

    let parse: Parser<AuthenticationValueRaw> =
        parser {
            let! tag = Tag.parse<Tag>

            match tag with
            | Tag.CharString ->
                return! Ber.GraphicString.parseImplicit |>> AuthenticationValueRaw.CharString

            | Tag.BitString ->
                return! Ber.BitString.parseImplicit|>> AuthenticationValueRaw.BitString

            | _ ->
                return! fail $"unexpected authentication-value choice tag {tag}"
        }

type AssociateSourceDiagnosticRaw =
    | AcseServiceUser of uint32
    | AcseServiceProvider of uint32

module AssociateSourceDiagnosticRaw =
    type Tag =
        | AcseServiceUser = 0xA1uy
        | AcseServiceProvider = 0xA2uy

    let parse : Parser<AssociateSourceDiagnosticRaw> =
        parser {
            let! tag = Tag.parse<Tag>

            match tag with
            | Tag.AcseServiceUser ->
                return!
                    Ber.parseLengthDelimited Ber.Integer.parseUint32
                    |>> AssociateSourceDiagnosticRaw.AcseServiceUser

            | Tag.AcseServiceProvider ->
                return!
                    Ber.parseLengthDelimited Ber.Integer.parseUint32
                    |>> AssociateSourceDiagnosticRaw.AcseServiceProvider

            | _ ->
                return! fail $"unexpected result-source-diagnostic choice tag {tag}"
        }