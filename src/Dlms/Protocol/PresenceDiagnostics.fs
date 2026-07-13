namespace Metering.Dlms.Protocol

open Metering.Common.Decoding.Validators.Core

type PresenceDiagnostic =
    | NoPresenceDiagnostic
    | InfoWhenPresent of string

module PresenceDiagnostic =

    let emit
        field diagnostic =
        match diagnostic with
        | NoPresenceDiagnostic ->
            passed ()

        | InfoWhenPresent message ->
            info field message