module Metering.Dlms.Protocol.Acse.Fields.OidValidation

open System
open Metering.Common.Validators.Core

let private validatePrefix
    (prefix: byte[])
    (fieldName: string)
    (value: ReadOnlyMemory<byte>)
    : Validation<unit> =

    let isValidPrefix =
        value.Span.Length >= prefix.Length
        && value.Span.Slice(0, prefix.Length).SequenceEqual(prefix.AsSpan())

    if isValidPrefix then
        Validation.ok ()
    else
        Validation.error $"{fieldName} object identifier has invalid prefix"

let private validateSingleId
    (prefix: byte[])
    (fieldName: string)
    (value: ReadOnlyMemory<byte>)
    : Validation<byte> =

    if value.Length = prefix.Length + 1 then
        Validation.ok value.Span[prefix.Length]
    else
        Validation.error $"{fieldName} object identifier must contain exactly one context id after the prefix"

let validatePrefixAndSingleId
    (prefix: byte[])
    (fieldName: string)
    (value: ReadOnlyMemory<byte>)
    : Validation<byte> =

    validator {
        let! _ =
            validatePrefix prefix fieldName value

        and! id =
            validateSingleId prefix fieldName value

        return id
    }