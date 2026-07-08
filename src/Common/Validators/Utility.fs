module Metering.Common.Decoding.Validators.Utility

open Metering.Common.Decoding.Validators.Core

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

