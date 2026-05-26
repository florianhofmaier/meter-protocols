namespace DlmsMessages.Acse

open DlmsMessages
open Mbus.BaseParsers.Core

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