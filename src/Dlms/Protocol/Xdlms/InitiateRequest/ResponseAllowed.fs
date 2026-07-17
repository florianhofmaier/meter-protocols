namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type ResponseAllowed =
    | ResponseAllowed
    | ResponseNotAllowed

module ResponseAllowed =

    let private fromBool value =
        if value then
            ResponseAllowed
        else
            ResponseNotAllowed

    let defaultValue = ResponseAllowed

    let validate
        (raw: Field<Axdr.Boolean>)
        : Validation<Field<ResponseAllowed>> =

        raw.Value
        |> Axdr.Boolean.value
        |> fromBool
        |> fun value -> Field.withValue value raw
        |> passed
