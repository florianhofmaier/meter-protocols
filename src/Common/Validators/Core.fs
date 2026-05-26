module Metering.Common.Validators.Core

open Metering.Common.Parsers.ParserTree

type ValidationResult<'a> =
    | Valid of 'a
    | Invalid of Failures

type VState =
    {
        CurrentNode : ParsedNode
        Notices : Notice list
    }

type Validation<'a> = VState -> ValidationResult<'a> * VState

module Validation =

    let private issue
        (message: string)
        (state: VState)
        : Issue =

        {
            Node = state.CurrentNode
            Message = message
        }

    let private addNotice
        (notice: Notice)
        (state: VState)
        : VState =

        { state with Notices = notice :: state.Notices }

    let ok
        (value: 'a)
        : Validation<'a> =

        fun state -> Valid value, state

    let error
        (message: string)
        : Validation<'a> =

        fun state ->
            let failure =
                state
                |> issue message
                |> Failures.create

            Invalid failure, state

    let info
        (message: string)
        : Validation<unit> =

        fun state ->
            let notice =
                state
                |> issue message
                |> Notice.Info

            Valid (), addNotice notice state

    let warning
        (message: string)
        : Validation<unit> =

        fun state ->
            let notice =
                state
                |> issue message
                |> Notice.Warning

            Valid (), addNotice notice state

    let ensure
        (message: string)
        (condition: bool)
        : Validation<unit> =

        if condition then
            ok ()
        else
            error message

    let requireSome
        (message: string)
        (value: 'a option)
        : Validation<'a> =

        match value with
        | Some value ->
            ok value

        | None ->
            error message

    let requireNone
        (message: string)
        (value: 'a option)
        : Validation<unit> =

        match value with
        | None ->
            ok ()

        | Some _ ->
            error message

    let withNode
        (node: ParsedNode)
        (validation: Validation<'a>)
        : Validation<'a> =

        fun state ->
            let previousNode =
                state.CurrentNode

            let innerState =
                { state with CurrentNode = node }

            let result, afterInner =
                validation innerState

            result, { afterInner with CurrentNode = previousNode }

    let parsed
        (raw: Parsed<'raw>)
        (validateValue: 'raw -> Validation<'valid>)
        : Validation<'valid> =

        validateValue raw.Value
        |> withNode raw.Node

    let runAtNode
        (node: ParsedNode)
        (validation: Validation<'a>)
        : ValidationReport<'a> =

        let initialState =
            {
                CurrentNode = node
                Notices = []
            }

        let result, finalState =
            validation initialState

        let notices =
            finalState.Notices |> List.rev

        match result with
        | Valid value ->
            Passed (value, notices)

        | Invalid failures ->
            Failed (failures, notices)

    let runParsed
        (validateValue: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw>)
        : ValidationReport<'valid> =

        validateValue raw.Value
        |> runAtNode raw.Node

    let map
        (f: 'a -> 'b)
        (validation: Validation<'a>)
        : Validation<'b> =

        fun state ->
            let result, state =
                validation state

            match result with
            | Valid value ->
                Valid (f value), state

            | Invalid failures ->
                Invalid failures, state

    let bind
        (f: 'a -> Validation<'b>)
        (validation: Validation<'a>)
        : Validation<'b> =

        fun state ->
            let result, state =
                validation state

            match result with
            | Valid value ->
                (f value) state

            | Invalid failures ->
                Invalid failures, state

    let merge
        (left: Validation<'a>)
        (right: Validation<'b>)
        : Validation<'a * 'b> =

        fun state ->
            let leftResult, state =
                left state

            let rightResult, state =
                right state

            match leftResult, rightResult with
            | Valid leftValue,
              Valid rightValue ->

                Valid (leftValue, rightValue), state

            | Invalid failures,
              Valid _ ->

                Invalid failures, state

            | Valid _,
              Invalid failures ->

                Invalid failures, state

            | Invalid leftFailures,
              Invalid rightFailures ->

                Invalid (Failures.append leftFailures rightFailures), state

    let sequenceOption
        (validation: Validation<'a> option)
        : Validation<'a option> =

        match validation with
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

    let sequenceList
        (validations: Validation<'a> list)
        : Validation<'a list> =

        fun state ->
            let initial : Failures option * 'a list * VState =
                None, [], state

            let folder
                (failuresSoFar, valuesSoFar, state)
                validation =

                let result, state =
                    validation state

                match failuresSoFar, result with
                | None, Valid value ->
                    None, value :: valuesSoFar, state

                | None, Invalid failures ->
                    Some failures, valuesSoFar, state

                | Some failuresSoFar, Valid _ ->
                    Some failuresSoFar, valuesSoFar, state

                | Some failuresSoFar, Invalid failures ->
                    Some (Failures.append failuresSoFar failures), valuesSoFar, state

            let failures, values, state =
                validations
                |> List.fold folder initial

            match failures with
            | None ->
                Valid (List.rev values), state

            | Some failures ->
                Invalid failures, state

    let traverseList
        (f: 'a -> Validation<'b>)
        (values: 'a list)
        : Validation<'b list> =

        values
        |> List.map f
        |> sequenceList

    let validateParsed
        (validateValue: 'raw -> Validation<'valid>)
        (raw: Parsed<'raw>)
        : ValidationReport<'valid> =

        raw |> runParsed validateValue

    type ValidationBuilder() =

        member _.Return
            (value: 'a)
            : Validation<'a> =

            ok value

        member _.ReturnFrom
            (validation: Validation<'a>)
            : Validation<'a> =

            validation

        member _.Bind
            (
                validation: Validation<'a>,
                f: 'a -> Validation<'b>
            )
            : Validation<'b> =

            bind f validation

        member _.MergeSources
            (
                left: Validation<'a>,
                right: Validation<'b>
            )
            : Validation<'a * 'b> =

            merge left right

        member _.BindReturn
            (
                validation: Validation<'a>,
                f: 'a -> 'b
            )
            : Validation<'b> =

            map f validation

        member _.Zero()
            : Validation<unit> =

            ok ()

        member _.Combine
            (
                first: Validation<unit>,
                second: Validation<'a>
            )
            : Validation<'a> =

            bind (fun () -> second) first

        member _.Delay
            (f: unit -> Validation<'a>)
            : Validation<'a> =

            f ()

let validator =
    Validation.ValidationBuilder()