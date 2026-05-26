namespace Metering.Common.Validators.Fields

open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Common.Validators.Core.Validation

type PresenceDiagnostic =
    | NoPresenceDiagnostic
    | InfoWhenPresent of string


module PresenceDiagnostic =

    let emit diagnostic : Validation<unit> =
        match diagnostic with
        | NoPresenceDiagnostic ->
            ok ()

        | InfoWhenPresent message ->
            info message

module FieldValidation =

    let fromParsedOptional
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw option>)
        : Validation<'valid option> =

        withNode raw.Node <|
            match raw.Value with
            | None ->
                ok None

            | Some value ->
                validator {
                    let! valid =
                        validatePresent value

                    do!
                        PresenceDiagnostic.emit presenceDiagnostic

                    return Some valid
                }

    let fromParsedRequired
        (missingMessage: string)
        (validatePresent: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw option>)
        : Validation<'valid> =

        withNode raw.Node <|
            validator {
                let! value =
                    requireSome missingMessage raw.Value

                return!
                    validatePresent value
            }

    let fromParsedRequiredKeepingSource
        (missingMessage: string)
        (validatePresent: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw option>)
        : Validation<Parsed<'valid>> =

        withNode raw.Node <|
            validator {
                let! value =
                    requireSome missingMessage raw.Value

                let! valid =
                    validatePresent value

                return {
                    Value = valid
                    Node = raw.Node
                }
            }

    let fromParsedDefaulted
        (defaultValue: 'valid)
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw option>)
        : Validation<'valid> =

        withNode raw.Node <|
            match raw.Value with
            | None ->
                ok defaultValue

            | Some value ->
                validator {
                    let! valid =
                        validatePresent value

                    do!
                        PresenceDiagnostic.emit presenceDiagnostic

                    return valid
                }