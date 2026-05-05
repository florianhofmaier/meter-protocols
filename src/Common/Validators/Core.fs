module Validators

type ValidationError =
    {
        Path : string list
        Message : string
    }

type Validation<'a> = Result<'a, ValidationError list>

module Validation =

    let ok value : Validation<'a> =
        Ok value

    let error path message : Validation<'a> =
        Error [
            {
                Path = path
                Message = message
            }
        ]

    let requireSome path message value : Validation<'a> =
        match value with
        | Some x -> Ok x
        | None -> error path message

    let requireNone path message value : Validation<unit> =
        match value with
        | None -> Ok ()
        | Some _ -> error path message

    let ensure path message condition : Validation<unit> =
        if condition then Ok ()
        else error path message

    let map f validation =
        match validation with
        | Ok x -> Ok (f x)
        | Error errors -> Error errors

    let bind f validation =
        match validation with
        | Ok x -> f x
        | Error errors -> Error errors

    let merge va vb =
        match va, vb with
        | Ok a, Ok b ->
            Ok (a, b)

        | Error ea, Ok _ ->
            Error ea

        | Ok _, Error eb ->
            Error eb

        | Error ea, Error eb ->
            Error (ea @ eb)

    let sequenceOption (value: Validation<'a> option) : Validation<'a option> =
        match value with
        | None ->
            ok None

        | Some validation ->
            validation |> map Some

    let traverseOption
        (f: 'a -> Validation<'b>)
        (value: 'a option)
        : Validation<'b option> =

        value
        |> Option.map f
        |> sequenceOption

    type ValidationBuilder() =

        member _.Return value : Validation<'a> =
            Ok value

        member _.ReturnFrom validation : Validation<'a> =
            validation

        member _.Bind(validation: Validation<'a>, f: 'a -> Validation<'b>) : Validation<'b> =
            bind f validation

        member _.Map(validation: Validation<'a>, f: 'a -> 'b) : Validation<'b> =
            map f validation

        member _.MergeSources
            (
                left: Validation<'a>,
                right: Validation<'b>
            ) : Validation<'a * 'b> =
            merge left right

        member _.BindReturn
            (
                validation: Validation<'a>,
                f: 'a -> 'b
            ) : Validation<'b> =
            map f validation

let validator = Validation.ValidationBuilder()