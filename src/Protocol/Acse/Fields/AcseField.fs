namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

module AcseField =

    let optional
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw option>)
        : Validation<'valid option> =

        validator {
            match raw.Value with
            | None ->
                return None

            | Some value ->
                let present =
                    {
                        Id = raw.Id
                        Span = raw.Span
                        Value = value
                    }

                let! valid =
                    validatePresent present

                do!
                    PresenceDiagnostic.emit raw presenceDiagnostic

                return Some valid
        }

    let required
        (missingMessage: string)
        (validatePresent: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw option>)
        : Validation<'valid> =

        validator {
            match raw.Value with
            | None ->
                return!
                    failed raw missingMessage

            | Some value ->
                let present: ParsedField<'raw> =
                    {
                        Id = raw.Id
                        Span = raw.Span
                        Value = value
                    }

                return!
                    validatePresent present
        }

    let requiredField
        (missingMessage: string)
        (validatePresent: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw option>)
        : Validation<ParsedField<'valid>> =

        validator {
            let! valid =
                required missingMessage validatePresent raw

            return
                {
                    Id = raw.Id
                    Span = raw.Span
                    Value = valid
                }
        }

    let defaulted
        (defaultValue: 'valid)
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw option>)
        : Validation<'valid> =

        validator {
            match raw.Value with
            | None ->
                return defaultValue

            | Some value ->
                let present: ParsedField<'raw> =
                    {
                        Id = raw.Id
                        Span = raw.Span
                        Value = value
                    }

                let! valid =
                    validatePresent present

                do!
                    PresenceDiagnostic.emit raw presenceDiagnostic

                return valid
        }

    let defaultedField
        (defaultValue: 'valid)
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: ParsedField<'raw> -> Validation<'valid>)
        (raw: ParsedField<'raw option>)
        : Validation<ParsedField<'valid>> =

        validator {
            let! valid =
                defaulted defaultValue presenceDiagnostic validatePresent raw

            return
                {
                    Id = raw.Id
                    Span = raw.Span
                    Value = valid
                }
        }
