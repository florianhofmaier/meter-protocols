namespace Metering.Dlms.ApduAnalyzer

open Metering.Common.Parsers
open Metering.Common.Parsers.Core
open Metering.Common.Validators
open Metering.Common.Validators.Core

type CosemError =
    | ParserError of ParserError
    | ValidationErrors of ParserError list

type DecoderStep<'a> =
    {
        Diagnostics : Diagnostic list
        Result : Result<'a, CosemError>
    }

module Decoder =

    let fromParseResult
        (result : Result<'a * ParserState, ParserError>)
        : DecoderStep<'a> =

        match result with
        | Ok (value, _) ->
            {
                Diagnostics = []
                Result = Ok value
            }

        | Result.Error parseError ->
            {
                Diagnostics = []
                Result = ParserError parseError |> Result.Error
            }

    let bindValidation
        (validate : Validation<'a>)
        (step : DecoderStep<'a>)
        : DecoderStep<'a> =

        match step.Result with
        | Result.Error error ->
            {
                Diagnostics = step.Diagnostics
                Result = error |> Result.Error
            }

        | Ok value ->
            let report =
                value
                |> Validation.run validate


            {
                Diagnostics =
                    step.Diagnostics @ report.Diagnostics

                Result =

            }