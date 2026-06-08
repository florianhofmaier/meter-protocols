namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol.Security.ProtectedApdus

type GloInitiateRequestValidatedFields =
    private ProtectedApdu of ProtectedApduValidatedFields

module GloInitiateRequestValidatedFields =

    let protectedApdu (ProtectedApdu protectedApdu) =
        protectedApdu

    let value (ProtectedApdu protectedApdu) =
        protectedApdu

    let fromRaw
        (raw: ParsedField<GloInitiateRequestRaw>)
        : Validation<GloInitiateRequestValidatedFields> =

        validator {
            return!
                raw.Value
                |> GloInitiateRequestRaw.value
                |> ProtectedApduValidatedFields.fromRaw ProtectedApduKind.ServiceSpecificGlobal
                |> map ProtectedApdu
        }