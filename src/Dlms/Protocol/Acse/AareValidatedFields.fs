namespace DlmsMessages.Acse

open Validators
open DlmsMessages.Utility

type AareValidatedFields =
    {
        ApplicationContextName : ApplicationContextName

        Result : AssociationResult
        ResultSourceDiagnostic : AssociateSourceDiagnostic

        RespondingApTitle : ApTitle option
        RespondingAeQualifier : AeQualifier option
        RespondingApInvocationId : InvocationIdentifier option
        RespondingAeInvocationId : InvocationIdentifier option

        Authentication : RespondingAuthentication
        ImplementationInformation : ImplementationInformation option
        UserInformation : BufferSlice
    }

module AareValidatedFields =
    let validate (raw: AareRawFields) : Validation<AareValidatedFields> =
        validator {
            let! _ =
                ProtocolVersion.validate raw.ProtocolVersion

            and! applicationContextName =
                raw.ApplicationContextName
                |> Validation.requireSome
                    ["AARE"; "application-context-name"]
                    "missing application-context-name"
                |> Validation.bind ApplicationContextName.validate

            and! result =
                raw.Result
                |> Validation.requireSome
                    ["AARE"; "result"]
                    "missing result"
                |> Validation.bind AssociationResult.fromRaw

            and! resultSourceDiagnostic =
                raw.ResultSourceDiagnostic
                |> Validation.requireSome
                    ["AARE"; "result-source-diagnostic"]
                    "missing result-source-diagnostic"
                |> Validation.bind AssociateSourceDiagnostic.fromRaw

            and! authentication =
                RespondingAuthentication.fromRaw
                    raw.ResponderAcseRequirements
                    raw.MechanismName
                    raw.RespondingAuthenticationValue

            and! userInformation =
                raw.UserInformation
                |> Validation.requireSome
                    ["AARE"; "user-information"]
                    "missing user-information"

            let respondingApTitle =
                raw.RespondingApTitle
                |> Option.map ApTitle.create

            let respondingAeQualifier =
                raw.RespondingAeQualifier
                |> Option.map AeQualifier.create

            let respondingApInvocationId =
                raw.RespondingApInvocationId
                |> Option.map InvocationIdentifier.create

            let respondingAeInvocationId =
                raw.RespondingAeInvocationId
                |> Option.map InvocationIdentifier.create

            let implementationInformation =
                raw.ImplementationInformation
                |> Option.map ImplementationInformation.create

            return {
                ApplicationContextName = applicationContextName

                Result = result
                ResultSourceDiagnostic = resultSourceDiagnostic

                RespondingApTitle = respondingApTitle
                RespondingAeQualifier = respondingAeQualifier
                RespondingApInvocationId = respondingApInvocationId
                RespondingAeInvocationId = respondingAeInvocationId

                Authentication = authentication
                ImplementationInformation = implementationInformation
                UserInformation = userInformation
            }
        }