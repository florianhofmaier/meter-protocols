namespace Metering.Dlms.Protocol.Security.ProtectedApdus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

type ProtectedApduValidatedFields =
    {
        SecurityControl : SecurityControl
        InvocationCounter : InvocationCounter
        Payload : ParsedField<ReadOnlyMemory<byte>>
    }

module ProtectedApduValidatedFields =

    let private validatePayload
        (payload: ParsedField<ReadOnlyMemory<byte>>)
        : Validation<ParsedField<ReadOnlyMemory<byte>>> =

        validator {
            if payload.Value.Length = 0 then
                return! failed payload "payload cannot be empty"
            else
                return payload
        }

    let fromRaw
        (apduKind: ProtectedApduKind)
        (raw: ParsedField<ProtectedApduRaw>)
        : Validation<ProtectedApduValidatedFields> =

        validator {
            let! securityControl =
                SecurityControl.fromParsed apduKind raw.Value.SecurityControl

            let! payload =
                validatePayload raw.Value.protectedPayload

            return {
                SecurityControl = securityControl
                InvocationCounter = raw.Value.InvocationCounter.Value
                Payload = payload
            }
        }