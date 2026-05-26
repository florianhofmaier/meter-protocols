namespace DlmsMessages.CosemServices.Open

open DlmsMessages.Xdlms
open DlmsMessages.Acse
open Mbus.BaseParsers.Core
open Validators

type OpenResponseRawFields =
    {
        Aare : AareRawFields
        UserInformation : XdlmsApduRaw
    }

type AareUserInformationRaw =
    | InitiateResponse of InitiateResponseRawFields
    | ConfirmedServiceError of ConfirmedServiceErrorRaw

module AareUserInformationRaw =
    let parse : Parser<AareUserInformationRaw> =
        parser {
            let! tag = Tag.parse<XdlmsTag>

            match tag with
            | XdlmsTag.InitiateResponse ->
                let! response = InitiateResponseRawFields.parseBody
                return AareUserInformationRaw.InitiateResponse response

            | XdlmsTag.ConfirmedServiceError ->
                let! error = ConfirmedServiceErrorRaw.parseBody
                return AareUserInformationRaw.ConfirmedServiceError error

            | other ->
                return! fail $"unexpected xDLMS APDU in AARE user-information: {other}"
        }

type AareUserInformation =
    | InitiateResponse of InitiateResponseValidatedFields
    | ConfirmedServiceError of ConfirmedServiceError

module AareUserInformation =
    let validateConfirmedServiceErrorForAare error =
        match error with
        | ConfirmedServiceError.InitiateError _ ->
            validationOkerror

        | ConfirmedServiceError.Read _
        | ConfirmedServiceError.Write _ ->
            validationError
                ["AARE"; "user-information"]
                "AARE confirmedServiceError must be initiateError"

    let fromRaw raw : Validation<AareUserInformation> =
        match raw with
        | AareUserInformationRaw.InitiateResponse response ->
            response
            |> InitiateResponseValidatedFields.fromRaw
            |> Validation.map AareUserInformation.InitiateResponse

        | AareUserInformationRaw.ConfirmedServiceError error ->
            error
            |> ConfirmedServiceError.fromRaw
            |> Validation.bind validateConfirmedServiceErrorForAare
            |> Validation.map AareUserInformation.ConfirmedServiceError

type OpenResponseValidatedFields =
    {
        Aare : AareValidatedFields
        UserInformation : AareUserInformation
    }

module OpenResponseValidatedFields =
    let validateConfirmedServiceErrorForOpenResponse error =
        match error with
        | ConfirmedServiceError.InitiateError _ ->
            validationOkerror

        | ConfirmedServiceError.Read _
        | ConfirmedServiceError.Write _ ->
            validationError
                ["COSEM-OPEN"; "AARE"; "user-information"]
                "AARE confirmedServiceError must be initiateError"

    let fromRaw (raw: OpenResponseRawFields) : Validation<OpenResponseValidatedFields> =
        validator {
            let! aare =
                AareValidatedFields.validate raw.Aare

            let! userInformation =
                match raw.UserInformation with
                | XdlmsApduRaw.InitiateResponse responseRaw ->
                    responseRaw
                    |> InitiateResponseValidatedFields.fromRaw
                    |> Validation.map AareUserInformation.InitiateResponse

                | XdlmsApduRaw.ConfirmedServiceError errorRaw ->
                    errorRaw
                    |> ConfirmedServiceError.fromRaw
                    |> Validation.bind validateConfirmedServiceErrorForOpenResponse
                    |> Validation.map AareUserInformation.ConfirmedServiceError

                | other ->
                    validationError
                        ["COSEM-OPEN"; "AARE"; "user-information"]
                        $"COSEM-OPEN.response user-information must contain initiateResponse or confirmedServiceError, got {other}"

            return {
                Aare = aare
                UserInformation = userInformation
            }
        }

type OpenResponseRejectedUserInformation =
    | InitiateResponse of InitiateResponseValidatedFields
    | InitiateError of ConfirmedServiceError

type OpenResponseAccepted =
    {
        ApplicationContextName : ApplicationContextName
        ResultSourceDiagnostic : AssociateSourceDiagnostic
        Authentication : RespondingAuthentication
        InitiateResponse : InitiateResponseValidatedFields
    }

type OpenResponseRejected =
    {
        ApplicationContextName : ApplicationContextName
        Result : AssociationResult
        ResultSourceDiagnostic : AssociateSourceDiagnostic
        Authentication : RespondingAuthentication
        UserInformation : OpenResponseRejectedUserInformation
    }

type OpenResponse =
    | Accepted of OpenResponseAccepted
    | Rejected of OpenResponseRejected

module OpenResponse =
    let fromValidatedFields
        (fields: OpenResponseValidatedFields)
        : Validation<OpenResponse> =

        validator {
            match fields.Aare.Result, fields.UserInformation with
            | AssociationResult.Accepted, AareUserInformation.InitiateResponse response ->
                return
                    Accepted {
                        ApplicationContextName = fields.Aare.ApplicationContextName
                        ResultSourceDiagnostic = fields.Aare.ResultSourceDiagnostic
                        Authentication = fields.Aare.Authentication
                        InitiateResponse = response
                    }

            | AssociationResult.Accepted, AareUserInformation.ConfirmedServiceError _ ->
                return!
                    validationError
                        ["COSEM-OPEN"; "AARE"; "user-information"]
                        "accepted AARE must carry initiateResponse"

            | AssociationResult.RejectedPermanent, AareUserInformation.InitiateResponse response
            | AssociationResult.RejectedTransient, AareUserInformation.InitiateResponse response ->
                return
                    Rejected {
                        ApplicationContextName = fields.Aare.ApplicationContextName
                        Result = fields.Aare.Result
                        ResultSourceDiagnostic = fields.Aare.ResultSourceDiagnostic
                        Authentication = fields.Aare.Authentication
                        UserInformation = OpenResponseRejectedUserInformation.InitiateResponse response
                    }

            | AssociationResult.RejectedPermanent, AareUserInformation.ConfirmedServiceError error
            | AssociationResult.RejectedTransient, AareUserInformation.ConfirmedServiceError error ->
                return
                    Rejected {
                        ApplicationContextName = fields.Aare.ApplicationContextName
                        Result = fields.Aare.Result
                        ResultSourceDiagnostic = fields.Aare.ResultSourceDiagnostic
                        Authentication = fields.Aare.Authentication
                        UserInformation = OpenResponseRejectedUserInformation.InitiateError error
                    }
        }

    let fromRaw raw =
        raw
        |> OpenResponseValidatedFields.fromRaw
        |> Validation.bind fromValidatedFields

