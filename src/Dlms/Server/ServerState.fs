namespace Metering.Dlms.Server

open Metering.Common.Validators
open Metering.Dlms.Protocol.ApplicationLayer

type ServerState =
    {
        CfState : CfState
    }

type ServerError =
    | InvalidInput
    | InvalidState
    | Rejected
    | InternalError

type ServerResponse =
    {
        State : ServerState
        Diagnostics : Diagnostic list
        Result : Result<XxDataRequest, ServerError>
    }