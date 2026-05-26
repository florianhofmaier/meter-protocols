namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open Metering.Common.Validators.Core
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
        (raw: Axdr.Boolean)
        : Validation<ResponseAllowed> =

        raw
        |> Axdr.Boolean.value
        |> fromBool
        |> Validation.ok