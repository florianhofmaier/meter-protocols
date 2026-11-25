module Metering.Common.Utility.Result

type ResultBuilder() =

    /// <summary>Wraps a returned value in an Ok result.</summary>
    member _.Return(value) = Ok value

    /// <summary>Uses an existing result as the result of the computation expression.</summary>
    member _.ReturnFrom(result: Result<_, _>) = result

    /// <summary>Passes an Ok value to the next computation; propagates an Error without invoking the continuation.</summary>
    member _.Bind(result: Result<'a, 'e>, next: 'a -> Result<'b, 'e>) =
        match result with
        | Ok value -> next value
        | Error error -> Error error

    /// <summary>Returns a successful unit result.</summary>
    member _.Zero() = Ok()

/// <summary>Builds result computation expressions that stop at the first Error.</summary>
let result = ResultBuilder()
