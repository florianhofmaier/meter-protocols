namespace DlmsMessages.Apdu

open DlmsMessages
open DlmsMessages.Xdlms
open DlmsMessages.Acse
open Mbus.BaseParsers.Core
open Validators

type AareUserInformationRaw =
    | InitiateResponse of InitiateResponseRaw
    | ConfirmedServiceError of ConfirmedServiceErrorRaw

module AareUserInformationRaw =
    let parse : Parser<AareUserInformationRaw> =
        parser {
            let! tag = Tag.parse<XdlmsTag>

            match tag with
            | XdlmsTag.InitiateResponse ->
                let! response = InitiateResponseRaw.parseBody
                return AareUserInformationRaw.InitiateResponse response

            | XdlmsTag.ConfirmedServiceError ->
                let! error = ConfirmedServiceErrorRaw.parseBody
                return AareUserInformationRaw.ConfirmedServiceError error

            | other ->
                return! fail $"unexpected xDLMS APDU in AARE user-information: {other}"
        }

type AareUserInformation =
    | InitiateResponse of InitiateResponse
    | ConfirmedServiceError of ConfirmedServiceError

module AareUserInformation =
    let validateConfirmedServiceErrorForAare error =
        match error with
        | ConfirmedServiceError.InitiateError _ ->
            Validation.ok error

        | ConfirmedServiceError.Read _
        | ConfirmedServiceError.Write _ ->
            Validation.error
                ["AARE"; "user-information"]
                "AARE confirmedServiceError must be initiateError"

    let fromRaw raw : Validation<AareUserInformation> =
        match raw with
        | AareUserInformationRaw.InitiateResponse response ->
            response
            |> InitiateResponse.fromRaw
            |> Validation.map AareUserInformation.InitiateResponse

        | AareUserInformationRaw.ConfirmedServiceError error ->
            error
            |> ConfirmedServiceError.fromRaw
            |> Validation.bind validateConfirmedServiceErrorForAare
            |> Validation.map AareUserInformation.ConfirmedServiceError

type AareAccepted =
    {
        ApplicationContextName : ApplicationContextName
        ResultSourceDiagnostic : AssociateSourceDiagnostic
        Authentication : RespondingAuthentication
        InitiateResponse : InitiateResponse
    }

type AareRejected =
    {
        ApplicationContextName : ApplicationContextName
        Result : AssociationResult
        ResultSourceDiagnostic : AssociateSourceDiagnostic
        Authentication : RespondingAuthentication
        InitiateError : ConfirmedServiceError
    }

type Aare =
    | Accepted of AareAccepted
    | Rejected of AareRejected

module Aare =
    let fromFields
        (fields: AareValidatedFields)
        (xdlms: AareUserInformation)
        : Validation<Aare> =

        validator {
            match fields.Result, xdlms with
            | AssociationResult.Accepted, AareUserInformation.InitiateResponse response ->
                return
                    Accepted {
                        ApplicationContextName = fields.ApplicationContextName
                        ResultSourceDiagnostic = fields.ResultSourceDiagnostic
                        Authentication = fields.Authentication
                        InitiateResponse = response
                    }

            | AssociationResult.Accepted, AareUserInformation.ConfirmedServiceError _ ->
                return!
                    Validation.error
                        ["AARE"; "user-information"]
                        "accepted AARE must carry InitiateResponse"

            | AssociationResult.RejectedPermanent, AareUserInformation.ConfirmedServiceError err
            | AssociationResult.RejectedTransient, AareUserInformation.ConfirmedServiceError err ->
                return
                    Rejected {
                        ApplicationContextName = fields.ApplicationContextName
                        Result = fields.Result
                        ResultSourceDiagnostic = fields.ResultSourceDiagnostic
                        Authentication = fields.Authentication
                        InitiateError = err
                    }

            | AssociationResult.RejectedPermanent, AareUserInformation.InitiateResponse _
            | AssociationResult.RejectedTransient, AareUserInformation.InitiateResponse _ ->
                return!
                    Validation.error
                        ["AARE"; "user-information"]
                        "rejected AARE must carry confirmedServiceError"
        }

