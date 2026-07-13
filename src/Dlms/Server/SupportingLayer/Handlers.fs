module Metering.Dlms.Server.SupportingLayer.Handlers

open System
open Metering.Dlms.Protocol.ApplicationLayer
open Metering.Dlms.Server
open Metering.Dlms.Server.SupportingLayer.TcpUdpIp

let private handleXxDataIndication
    (policy: ServerPolicy)
    (state: ServerState)
    (indication: XxDataIndication)
    (diagnostics: Diagnostic list)
    : ServerResponse =

    let openResult =
        ServerCf.xxDataIndication
            state.CfState
            indication

    let diagnostics =
        diagnostics @ openResult.Diagnostics

    match openResult.Result with
    | Error error ->
        {
            State = { state with CfState = openResult.State }
            Diagnostics = diagnostics
            Result = Error error
        }

    | Ok openIndication ->
        let openResponse =
            policy.DecideOpen openIndication

        let responseResult =
            ServerCf.cosemOpenResponse
                openResult.State
                openResponse

        {
            State = { state with CfState = responseResult.State }
            Diagnostics = diagnostics @ responseResult.Diagnostics
            Result = responseResult.Result
        }

let onBytes
    (policy: ServerPolicy)
    (state: ServerState)
    (bytes: ReadOnlyMemory<byte>)
    : ServerResponse =

    match ParserRunner.runOnBytes WrapperPduRaw.parse bytes with
    | Error parseError ->
        {
            State = state
            Diagnostics = [ Diagnostic.fromParseError parseError ]
            Result = Error InvalidInput
        }

    | Ok wrapperRaw ->
        let indicationReport =
            ServerWrapperProfile.toXxDataIndication wrapperRaw
            |> Validation.run

        match indicationReport.Result with
        | Error () ->
            {
                State = state
                Diagnostics = indicationReport.Diagnostics
                Result = Error InvalidInput
            }

        | Ok xxDataIndication ->
            handleXxDataIndication
                policy
                state
                xxDataIndication
                indicationReport.Diagnostics