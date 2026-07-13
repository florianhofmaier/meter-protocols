namespace Metering.Dlms.Protocol.Security.ProtectedApdus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility

type ProtectedApduRaw =
    {
        SecurityControl : ParsedField<SecurityControlRaw>
        InvocationCounter : ParsedField<InvocationCounter>
        protectedPayload : ParsedField<ReadOnlyMemory<byte>>
    }

module ProtectedApduRaw =

    let parse : Parser<ParsedField<ProtectedApduRaw>> =
        parseField "protected-apdu"
        <| parser {
            let! securityControl =
                SecurityControlRaw.parse

            let! invocationCounter =
                InvocationCounter.parse

            let! protectedPayload =
                parseField "protected-payload"
                <| takeAll

            return {
                SecurityControl = securityControl
                InvocationCounter = invocationCounter
                protectedPayload = protectedPayload
            }
        }