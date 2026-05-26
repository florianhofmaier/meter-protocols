module Metering.Dlms.Server.ServerCf

open Metering.Common.Parsers
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators
open Metering.Dlms.Protocol.Acse.Aarq
open Metering.Dlms.Protocol.Acse.Fields
open Metering.Dlms.Protocol.ApplicationLayer
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.InitiateRequest

let private decodeInitiateRequest
    (indication: XxDataIndication)
    (aarq: AarqValidatedFields)
    (diagnostics: Diagnostic list)
    : ServerCfTransition<CosemOpenIndication> =

    let initiateRequestRaw =
        aarq.UserInformation
        |> Parsed.map UserInformation.value
        |> ParserRunner.runOnParsedMemory InitiateRequestRaw.parse

    match initiateRequestRaw with
    | Error parseError ->
        {
            State = { CfState = CfState.Idle }
            Diagnostics = diagnostics @ [ Diagnostic.fromParseError parseError ]
            Result = Error InvalidInput
        }

    | Ok initiateRaw ->
        let initiateReport =

            Validation.run (InitiateRequestValidatedFields.fromRaw initiateRaw)


        match initiateReport.Result with
        | Error () ->
            {
                State = CfState.Idle
                Diagnostics = diagnostics @ initiateReport.Diagnostics
                Result = Error InvalidInput
            }

        | Ok initiateRequest ->
            let openIndication =
                {
                    Address = indication.Address
                    Aarq = aarq
                    InitiateRequest = initiateRequest
                }

            {
                State =
                    CfState.AssociationPending {
                        Address = indication.Address
                        OpenIndication = openIndication
                    }

                Diagnostics = diagnostics @ initiateReport.Diagnostics
                Result = Ok openIndication
            }

let private decodeOpenIndication
    (indication: XxDataIndication)
    : ServerCfTransition<CosemOpenIndication> =

    match ParserRunner.runOnParsedMemory AarqRaw.parse indication.Payload with
    | Error parseError ->
        {
            State = CfState.Idle
            Diagnostics = [ Diagnostic.fromParseError parseError ]
            Result = Error InvalidInput
        }

    | Ok aarqRaw ->
        let aarqReport =
            AarqValidatedFields.fromParsed aarqRaw
            |> Validation.run

        match aarqReport.Result with
        | Error () ->
            {
                State = CfState.Idle
                Diagnostics = aarqReport.Diagnostics
                Result = Error InvalidInput
            }

        | Ok aarq ->
            decodeInitiateRequest
                indication
                aarq
                aarqReport.Diagnostics

let xxDataIndication
    (state: CfState)
    (indication: XxDataIndication)
    : ServerCfTransition<CosemOpenIndication> =

    match state with
    | CfState.Idle ->
        decodeOpenIndication indication

    | _ ->
        {
            State = state
            Diagnostics = [
                Diagnostic.error "XX-DATA.indication is not valid in the current CF state"
            ]
            Result = Error InvalidState
        }

let cosemOpenResponse
    (state: CfState)
    (response: CosemOpenResponse)
    : ServerCfTransition<XxDataRequest> =

    match state with
    | CfState.AssociationPending pending ->

        let aare =
            AareBuilder.fromOpenResponse pending response

        let xxDataRequest =
            {
                Address = pending.Address
                Payload = AareWriter.write aare
            }

        let nextState =
            match response with
            | CosemOpenResponse.Accept _ ->
                CfState.Associated {
                    Address = pending.Address
                    // negotiated context etc.
                }

            | CosemOpenResponse.Reject _ ->
                CfState.Idle

        {
            State = nextState
            Diagnostics = []
            Result = Ok xxDataRequest
        }

    | _ ->
        {
            State = state
            Diagnostics = [
                Diagnostic.error "COSEM-OPEN.response is not valid in the current CF state"
            ]
            Result = Error InvalidState
        }