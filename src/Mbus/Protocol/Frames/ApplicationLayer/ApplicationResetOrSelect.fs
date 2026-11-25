namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core

type ApplicationResetOrSelectRaw =
    | ApplicationResetRaw
    | ApplicationSelectRaw of ReadOnlyMemory<byte>

module ApplicationResetOrSelectRaw =

    let parse: Parser<Field<ApplicationResetOrSelectRaw>> =
        parseField "APL Data"
        <| parser {
            let! remaining = remaining

            if remaining = 0 then
                return ApplicationResetRaw
            else
                return! takeAll |>> ApplicationSelectRaw
        }

type ApplicationResetOrSelect =
    | ApplicationReset
    | ApplicationSelect of ReadOnlyMemory<byte>

module ApplicationResetOrSelect =

    let private maxSubcodeBytes = 10

    let validate (raw: Field<ApplicationResetOrSelectRaw>) : Validation<ApplicationResetOrSelect> =

        validator {
            match raw.Value with
            | ApplicationResetRaw -> return ApplicationReset

            | ApplicationSelectRaw bytes ->
                let! () =
                    ensure
                        raw
                        $"Application select subcode is expected to be at most {maxSubcodeBytes} byte(s), but it's {bytes.Length} byte(s)"
                        (bytes.Length <= maxSubcodeBytes)

                return ApplicationSelect bytes
        }

    /// Writes application reset or the bounded application-selection subcode.
    let write (selection: ApplicationResetOrSelect) : Writer<unit> =
        match selection with
        | ApplicationReset -> Writer.ret ()
        | ApplicationSelect bytes when bytes.IsEmpty || bytes.Length > maxSubcodeBytes ->
            writerError "Application selection requires one to ten subcode bytes"
        | ApplicationSelect bytes -> writeMemory bytes

    /// EN 13757-3:2025, 7.2: application nibbles and up to twelve block-number bits.
    /// Unsupported extended selections fall back to application reset (7.4.2).
    let selection =
        function
        | ApplicationReset -> 0, 0
        | ApplicationSelect bytes when bytes.Length > 3 -> -1, 0
        | ApplicationSelect bytes ->
            let mutable application = 0
            let mutable block = 0
            let mutable extended = true

            for i in 0 .. bytes.Length - 1 do
                let value = int bytes.Span[i]

                if extended then
                    application <- application + (value >>> 4)
                    extended <- value >>> 4 = 15

                block <- block ||| ((value &&& 15) <<< (4 * i))

            application, block
