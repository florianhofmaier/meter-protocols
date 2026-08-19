module Metering.Common.Decoding.Validators.Utility

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

let validateField
    (validate: Field<'raw> -> Validation<'valid>)
    (raw: Field<'raw>)
    : Validation<Field<'valid>> =

    validate raw
    |> map (fun valid ->
        raw |> Field.withValue valid)

let sequence
    (items: Validation<'a> list)
    : Validation<'a list> =

    let appendHead item state =
        merge item state
        |> map (fun (head, tail) -> head :: tail)

    items
    |> List.foldBack appendHead <| passed []

let traverse
    (f: 'a -> Validation<'b>)
    (items: 'a list)
    : Validation<'b list> =
    items
    |> List.map f
    |> sequence

let collectIssues
    (items: Validation<unit> list)
    : Validation<unit> =

    items
    |> sequence
    |> map ignore

