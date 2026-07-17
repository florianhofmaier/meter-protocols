namespace Metering.Dlms.Protocol.Security.ProtectedApdus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

type ProtectedApduValidatedFields =
    {
        SecurityControl : Field<SecurityControl>
        InvocationCounter : Field<InvocationCounter>
        Payload : Field<ReadOnlyMemory<byte>>
    }

module ProtectedApduValidatedFields =

    let private validatePayload
        (payload: Field<ReadOnlyMemory<byte>>)
        : Validation<Field<ReadOnlyMemory<byte>>> =

        validator {
            if payload.Value.Length = 0 then
                return! failed payload "payload cannot be empty"
            else
                return payload
        }

    let fromRaw
        (apduKind: ProtectedApduKind)
        (raw: Field<ProtectedApduRaw>)
        : Validation<Field<ProtectedApduValidatedFields>> =

        validator {
            let! securityControl =
                SecurityControl.fromParsed apduKind raw.Value.SecurityControl

            let! payload =
                validatePayload raw.Value.protectedPayload

            return
                raw
                |> Field.withValue {
                    SecurityControl = securityControl
                    InvocationCounter = raw.Value.InvocationCounter
                    Payload = payload
                }
        }
