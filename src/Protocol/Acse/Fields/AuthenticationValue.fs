namespace Metering.Dlms.Protocol.Acse.Fields

open System
open Metering.Common.Parsers.Core
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type AuthenticationValueRaw =
    | CharString of Ber.GraphicString
    | BitString of Ber.BitString

module AuthenticationValueRaw =

    type ChoiceTag =
        | CharString = 0x80uy
        | BitString = 0x81uy

    let parse: Parser<AuthenticationValueRaw> =
        parser {
            let! tag = Tag.parse<ChoiceTag>

            match tag with
            | ChoiceTag.CharString ->
                return! Ber.GraphicString.parseImplicit |>> AuthenticationValueRaw.CharString

            | ChoiceTag.BitString ->
                return! Ber.BitString.parseImplicit|>> AuthenticationValueRaw.BitString

            | _ ->
                return! fail $"unexpected authentication-value choice tag {tag}"
        }

type AuthenticationValue =
    | CharString of ReadOnlyMemory<byte>
    | BitString of Ber.BitString

module AuthenticationValue =

    let validate
        (raw: AuthenticationValueRaw)
        : Validation<AuthenticationValue> =

        validator {
            match raw with
            | AuthenticationValueRaw.CharString value ->
                return
                    value
                    |> Ber.GraphicString.toBytes
                    |> AuthenticationValue.CharString

            | AuthenticationValueRaw.BitString bits ->
                return AuthenticationValue.BitString bits
        }