module Metering.Dlms.Protocol.Acse.Fields.OidValidation

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

module OidValidation =

    let private validatePrefix
        (prefix: byte[])
        (fieldName: string)
        (raw: Field<Ber.ObjectIdentifier>)
        : Validation<unit> =

        let bytes =
            Ber.ObjectIdentifier.toBytes raw.Value

        let isValidPrefix =
            bytes.Length >= prefix.Length
            && bytes.Span.Slice(0, prefix.Length).SequenceEqual(prefix.AsSpan())

        if isValidPrefix then
            passed ()
        else
            failed raw $"{fieldName} object identifier has invalid prefix"

    let private validateSingleId
        (prefix: byte[])
        (fieldName: string)
        (raw: Field<Ber.ObjectIdentifier>)
        : Validation<byte> =

        let bytes =
            Ber.ObjectIdentifier.toBytes raw.Value

        if bytes.Length = prefix.Length + 1 then
            passed bytes.Span[prefix.Length]
        else
            failed raw $"{fieldName} object identifier must contain exactly one context id after the prefix"

    let validatePrefixAndSingleId
        (prefix: byte[])
        (fieldName: string)
        (raw: Field<Ber.ObjectIdentifier>)
        : Validation<byte> =

        validator {
            let! () =
                validatePrefix prefix fieldName raw

            and! id =
                validateSingleId prefix fieldName raw

            return id
        }