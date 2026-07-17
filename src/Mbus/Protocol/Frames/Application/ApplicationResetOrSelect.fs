namespace Metering.Mbus.Protocol.Frames.Application

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type ApplicationResetOrSelectRaw =
    | ApplicationResetRaw
    | ApplicationSelectRaw of Field<ReadOnlyMemory<byte>>

module ApplicationResetOrSelectRaw =

    let parse : Parser<Field<ApplicationResetOrSelectRaw>> =
        parseField "Application Reset/Select"
        <| parser {
            let! remaining = remaining

            if remaining = 0 then
                return ApplicationResetRaw
            else
                let! subcode =
                    parseField "Application Select Subcode" takeAll

                return ApplicationSelectRaw subcode
        }

type ApplicationResetOrSelect =
    | ApplicationReset
    | ApplicationSelect of ReadOnlyMemory<byte>

module ApplicationResetOrSelect =

    let private maxSubcodeBytes = 10

    let fromRaw
        (raw: Field<ApplicationResetOrSelectRaw>)
        : Validation<ApplicationResetOrSelect> =

        validator {
            match raw.Value with
            | ApplicationResetRaw ->
                return ApplicationReset

            | ApplicationSelectRaw subcode ->
                let bytes = subcode.Value

                let! () =
                    ensure
                        subcode
                        $"Application select subcode is expected to be at most {maxSubcodeBytes} byte(s), but it's {bytes.Length} byte(s)"
                        (bytes.Length <= maxSubcodeBytes)

                return ApplicationSelect bytes
        }
