namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Fields.OidValidation

type ApplicationContextName =
    | LogicalNameNoCiphering
    | ShortNameNoCiphering
    | LogicalNameWithCiphering
    | ShortNameWithCiphering

module ApplicationContextName =

    let parse : Parser<Ber.ObjectIdentifier> =
        Ber.ObjectIdentifier.parse

    let parseContent : Parser<Ber.ObjectIdentifier> =
        Ber.ObjectIdentifier.parseContent

    let validate
        (raw: Field<Ber.ObjectIdentifier>)
        : Validation<ApplicationContextName> =

        validator {
            let! contextName =
                OidValidation.validatePrefixAndSingleId
                    <| [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x01uy |]
                    <| "application-context-name"
                    <| raw

            match contextName with
            | 0x01uy ->
                return LogicalNameNoCiphering

            | 0x02uy ->
                return ShortNameNoCiphering

            | 0x03uy ->
                return LogicalNameWithCiphering

            | 0x04uy ->
                return ShortNameWithCiphering

            | value ->
                return!
                    failed
                        <| raw
                        <| $"unsupported application-context-name id 0x{value:X2}"
        }