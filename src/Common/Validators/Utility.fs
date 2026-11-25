module Metering.Common.Decoding.Validators.Utility

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

/// <summary>Validates a field's value and wraps a successful result with the original identity and source span, preserving diagnostics.</summary>
let validateField
    (validate: Field<'raw> -> Validation<'valid>)
    (raw: Field<'raw>)
    : Validation<Field<'valid>> =

    validate raw |> map (fun valid -> raw |> Field.withValue valid)

/// <summary>Combines validation results in list order, accumulating all independent failures and notices.</summary>
let sequence (items: Validation<'a> list) : Validation<'a list> =

    let appendHead item state =
        merge item state |> map (fun (head, tail) -> head :: tail)

    items |> List.foldBack appendHead <| passed []

/// <summary>Validates every item in list order and combines the results, accumulating independent failures and notices.</summary>
let traverse (f: 'a -> Validation<'b>) (items: 'a list) : Validation<'b list> =
    items |> List.map f |> sequence

/// <summary>Combines unit validation results, collecting all failures and notices in list order.</summary>
let collectIssues (items: Validation<unit> list) : Validation<unit> =

    items |> sequence |> map ignore
