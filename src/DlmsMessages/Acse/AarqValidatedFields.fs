namespace DlmsMessages.Acse

open Validators

type AarqValidatedFields =
    {
        ApplicationContextName : ApplicationContextName

        CalledApTitle : ApTitle option
        CalledAeQualifier : AeQualifier option
        CalledApInvocationId : InvocationIdentifier option
        CalledAeInvocationId : InvocationIdentifier option

        CallingApTitle : ApTitle option
        CallingAeQualifier : AeQualifier option
        CallingApInvocationId : InvocationIdentifier option
        CallingAeInvocationId : InvocationIdentifier option

        Authentication : Authentication
        ImplementationInformation : ImplementationInformation option
        UserInformation : UserInformation
    }

module AarqValidatedFields =
    let validate (raw: AarqRawFields) : Validation<AarqValidatedFields> =
        validator {
            let! _ =
                ProtocolVersion.validate raw.ProtocolVersion

            and! applicationContextName =
                raw.ApplicationContextName
                |> Validation.requireSome
                    ["AARE"; "application-context-name"]
                    "missing application-context-name"
                |> Validation.bind ApplicationContextName.validate

            and! authentication =
                Authentication.validate
                    raw.SenderAcseRequirements
                    raw.MechanismName
                    raw.CallingAuthenticationValue

            and! userInformation =
                UserInformation.validate raw.UserInformation

            let calledApTitle =
                raw.CalledApTitle
                |> Option.map ApTitle.create

            let calledAeQualifier =
                raw.CalledAeQualifier
                |> Option.map AeQualifier.create

            let calledApInvocationId =
                raw.CalledApInvocationId
                |> Option.map InvocationIdentifier.create

            let calledAeInvocationId =
                raw.CalledAeInvocationId
                |> Option.map InvocationIdentifier.create

            let callingApTitle =
                raw.CallingApTitle
                |> Option.map ApTitle.create

            let callingAeQualifier =
                raw.CallingAeQualifier
                |> Option.map AeQualifier.create

            let callingApInvocationId =
                raw.CallingApInvocationId
                |> Option.map InvocationIdentifier.create

            let callingAeInvocationId =
                raw.CallingAeInvocationId
                |> Option.map InvocationIdentifier.create

            let implementationInformation =
                raw.ImplementationInformation
                |> Option.map ImplementationInformation.create

            return {
                ApplicationContextName = applicationContextName

                CalledApTitle = calledApTitle
                CalledAeQualifier = calledAeQualifier
                CalledApInvocationId = calledApInvocationId
                CalledAeInvocationId = calledAeInvocationId

                CallingApTitle = callingApTitle
                CallingAeQualifier = callingAeQualifier
                CallingApInvocationId = callingApInvocationId
                CallingAeInvocationId = callingAeInvocationId

                Authentication = authentication
                ImplementationInformation = implementationInformation
                UserInformation = userInformation
            }
        }