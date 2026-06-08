module Metering.Common.Utility.Result

type ResultBuilder() =

    member _.Return(value) =
        Ok value

    member _.ReturnFrom(result: Result<_, _>) =
        result

    member _.Bind(result: Result<'a, 'e>, next: 'a -> Result<'b, 'e>) =
        match result with
        | Ok value -> next value
        | Error error -> Error error

    member _.Zero() =
        Ok ()

let result =
    ResultBuilder()