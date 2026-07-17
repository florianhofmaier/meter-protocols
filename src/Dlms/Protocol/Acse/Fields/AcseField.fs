namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

module AcseField =

    let optional
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
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

    let optionalField
        (presenceDiagnostic: PresenceDiagnostic)
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
        : Validation<Field<'valid option>> =

        validator {
            let! valid =
                optional presenceDiagnostic validatePresent raw

            return raw |> Field.withValue valid
        }

    let required
        (missingMessage: string)
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
        : Validation<'valid> =

        validator {
            match raw.Value with
            | None ->
                return!
                    failed raw missingMessage

            | Some value ->
                let present: Field<'raw> =
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
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
        : Validation<Field<'valid>> =

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
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
        : Validation<'valid> =

        validator {
            match raw.Value with
            | None ->
                return defaultValue

            | Some value ->
                let present: Field<'raw> =
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
        (validatePresent: Field<'raw> -> Validation<'valid>)
        (raw: Field<'raw option>)
        : Validation<Field<'valid>> =

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
