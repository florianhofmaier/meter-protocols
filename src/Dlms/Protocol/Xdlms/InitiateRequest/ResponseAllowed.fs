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
        (raw: ParsedField<Axdr.Boolean>)
        : Validation<ResponseAllowed> =

        raw.Value
        |> Axdr.Boolean.value
        |> fromBool
        |> passed