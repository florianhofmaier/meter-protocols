namespace DlmsMessages.CosemServices.Open

open System
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol.Acse.Aarq

// type OpenRequestRawFields =
//     private
//             Aarq of AarqRawFields

module OpenRequestRawFields =
    let parseBody: Parser<OpenRequestRawFields> =
        AarqRaw.parseBody
        |>> OpenRequestRawFields.Aarq

type OpenRequestValidatedFields =
    {
        Aarq : AarqValidatedFields
        InitiateRequest : OpenRequestUserInformation
    }

module OpenRequestValidatedFields =

    let fromRaw
        (raw: Field<OpenRequestRawFields>)
        : Validation<OpenRequestValidatedFields> =

        validator {
            let! aarq =
                Validation.parsed
                    raw
                    AarqValidatedFields.validate


            match raw.UserInformation with
            | Xdlms.XdlmsApduRaw.InitiateRequest initiateRaw ->
                let! initiate =
                    InitiateRequestValidatedFields.fromRaw initiateRaw

                return {
                    Aarq = aarq
                    InitiateRequest = initiate
                }

            | _ ->
                return!
                    validationError
                        ["COSEM-OPEN"; "AARQ"; "user-information"]
                        "COSEM-OPEN.request user-information must resolve to initiateRequest"
        }

type LogicalNameConformance =
    private LogicalNameConformance of Conformance

module LogicalNameConformance =
    let fromConformance (c: Conformance) : Validation<LogicalNameConformance> =
        validationOk(LogicalNameConformance c)

    let private value (LogicalNameConformance c) = c

    let generalProtection x =
        x |> value |> Conformance.generalProtection

    let generalBlockTransfer x =
        x |> value |> Conformance.generalBlockTransfer

    let deltaValueEncoding x =
        x |> value |> Conformance.deltaValueEncoding

    let attribute0SupportedWithSet x =
        x |> value |> Conformance.attribute0SupportedWithSet

    let priorityMgmtSupported x =
        x |> value |> Conformance.priorityMgmtSupported

    let attribute0SupportedWithGet x =
        x |> value |> Conformance.attribute0SupportedWithGet

    let blockTransferWithGetOrRead x =
        x |> value |> Conformance.blockTransferWithGetOrRead

    let blockTransferWithSetOrWrite x =
        x |> value |> Conformance.blockTransferWithSetOrWrite

    let blockTransferWithAction x =
        x |> value |> Conformance.blockTransferWithAction

    let multipleReferences x =
        x |> value |> Conformance.multipleReferences

    let dataNotification x =
        x |> value |> Conformance.dataNotification

    let access x =
        x |> value |> Conformance.access

    let get x =
        x |> value |> Conformance.get

    let set x =
        x |> value |> Conformance.set

    let selectiveAccess x =
        x |> value |> Conformance.selectiveAccess

    let eventNotification x =
        x |> value |> Conformance.eventNotification

    let action x =
        x |> value |> Conformance.action


type LogicalNameNoCipheringInitiateRequest =
    {
        ResponseAllowed : ResponseAllowed
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : LogicalNameConformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module LogicalNameNoCipheringInitiateRequest =
    let fromInitiateRequest
        (request: InitiateRequestValidatedFields)
        : Validation<LogicalNameNoCipheringInitiateRequest> =
        validator {
            do!
                request.DedicatedKey
                |> Validation.requireNone
                    ["InitiateRequest"; "dedicated-key"]
                    "dedicated-key is not allowed in a no-ciphering application context"

            let! conformance =
                LogicalNameConformance.fromConformance request.ProposedConformance

            return {
                ResponseAllowed = request.ResponseAllowed
                ProposedDlmsVersionNumber = request.ProposedDlmsVersionNumber
                ProposedConformance = conformance
                ClientMaxReceivePduSize = request.ClientMaxReceivePduSize
            }
        }

type ShortNameConformance =
    private ShortNameConformance of Conformance

module ShortNameConformance =
    let fromConformance (c: Conformance) : Validation<ShortNameConformance> =
        validationOk(ShortNameConformance c)

    let private value (ShortNameConformance c) = c

    let generalProtection x =
        x |> value |> Conformance.generalProtection

    let generalBlockTransfer x =
        x |> value |> Conformance.generalBlockTransfer

    let read x =
        x |> value |> Conformance.read

    let write x =
        x |> value |> Conformance.write

    let unconfirmedWrite x =
        x |> value |> Conformance.unconfirmedWrite

    let deltaValueEncoding x =
        x |> value |> Conformance.deltaValueEncoding

    let blockTransferWithGetOrRead x =
        x |> value |> Conformance.blockTransferWithGetOrRead

    let blockTransferWithSetOrWrite x =
        x |> value |> Conformance.blockTransferWithSetOrWrite

    let multipleReferences x =
        x |> value |> Conformance.multipleReferences

    let informationReport x =
        x |> value |> Conformance.informationReport

    let dataNotification x =
        x |> value |> Conformance.dataNotification

    let parameterizedAccess x =
        x |> value |> Conformance.parameterizedAccess

type ClientSystemTitle =
    private ClientSystemTitle of ReadOnlyMemory<byte>

module ClientSystemTitle =
    let value (ClientSystemTitle bytes) =
        bytes

    let private validate (bytes: ReadOnlyMemory<byte>) : Validation<ClientSystemTitle> =
        if bytes.Length = 8 then
            validationOk(ClientSystemTitle bytes)
        else
            validationError
                ["AARQ"; "calling-AP-title"]
                $"client system title must be 8 octets, got {bytes.Length}"

    let fromApTitle apTitle : Validation<ClientSystemTitle> =
        apTitle
        |> ApTitle.value
        |> Ber.OctetString.toBytes
        |> validate

type ClientDigitalSignatureCertificate =
    private ClientDigitalSignatureCertificate of ReadOnlyMemory<byte>

module ClientDigitalSignatureCertificate =
    let value (ClientDigitalSignatureCertificate bytes) =
        bytes

    let private validate (bytes: ReadOnlyMemory<byte>) : Validation<ClientDigitalSignatureCertificate> =
        if bytes.Length = 0 then
            validationError
                ["AARQ"; "calling-AE-qualifier"]
                "client digital signature certificate must not be empty"
        else
            validationOk(ClientDigitalSignatureCertificate bytes)

    let fromAeQualifier (aeQualifier: AeQualifier) : Validation<ClientDigitalSignatureCertificate> =
        aeQualifier
            |> AeQualifier.value
            |> Ber.OctetString.toBytes
            |> validate

type ClientUserIdentifier =
    private ClientUserIdentifier of uint32

module ClientUserIdentifier =
    let create value =
        ClientUserIdentifier value

    let value (ClientUserIdentifier value) =
        value

    let fromInvocationIdentifier (identifier: InvocationIdentifier) =
        identifier
        |> InvocationIdentifier.value
        |> create

type LogicalNameNoCipheringOpenRequest =
    {
        ClientUserIdentifier : ClientUserIdentifier option
        Authentication : Authentication
        InitiateRequest : LogicalNameNoCipheringInitiateRequest
    }

module LogicalNameNoCipheringOpenRequest =
    let fromFields (aarq: AarqValidatedFields) (initiate: InitiateRequestValidatedFields) : Validation<
                                                                                                LogicalNameNoCipheringOpenRequest
                                                                                                > =
        validator {
            let! initiate = LogicalNameNoCipheringInitiateRequest.fromInitiateRequest initiate
            let userId =
                aarq.CallingAeInvocationId
                |> Option.map ClientUserIdentifier.fromInvocationIdentifier

            return {
                ClientUserIdentifier = userId
                Authentication = aarq.Authentication
                InitiateRequest = initiate
            }
        }

type LogicalNameWithCipheringInitiateRequest =
    {
        DedicatedKey : DedicatedKey option
        ResponseAllowed : ResponseAllowed
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : LogicalNameConformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module LogicalNameWithCipheringInitiateRequest =
    let fromInitiateRequest
        (request: InitiateRequestValidatedFields)
        : Validation<LogicalNameWithCipheringInitiateRequest> =
        validator {
            let! conformance =
                LogicalNameConformance.fromConformance request.ProposedConformance

            return {
                DedicatedKey = request.DedicatedKey
                ResponseAllowed = request.ResponseAllowed
                ProposedDlmsVersionNumber = request.ProposedDlmsVersionNumber
                ProposedConformance = conformance
                ClientMaxReceivePduSize = request.ClientMaxReceivePduSize
            }
        }

type ShortNameNoCipheringInitiateRequest =
    {
        ResponseAllowed : ResponseAllowed
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : ShortNameConformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module ShortNameNoCipheringInitiateRequest =
    let fromInitiateRequest
        (request: InitiateRequestValidatedFields)
        : Validation<ShortNameNoCipheringInitiateRequest> =
        validator {
            do!
                request.DedicatedKey
                |> Validation.requireNone
                    ["InitiateRequest"; "dedicated-key"]
                    "dedicated-key is not allowed in a no-ciphering application context"

            let! conformance =
                ShortNameConformance.fromConformance request.ProposedConformance

            return {
                ResponseAllowed = request.ResponseAllowed
                ProposedDlmsVersionNumber = request.ProposedDlmsVersionNumber
                ProposedConformance = conformance
                ClientMaxReceivePduSize = request.ClientMaxReceivePduSize
            }
        }

type ShortNameWithCipheringInitiateRequest =
    {
        DedicatedKey : DedicatedKey option
        ResponseAllowed : ResponseAllowed
        ProposedDlmsVersionNumber : DlmsVersionNumber
        ProposedConformance : ShortNameConformance
        ClientMaxReceivePduSize : ClientMaxReceivePduSize
    }

module ShortNameWithCipheringInitiateRequest =
    let fromInitiateRequest
        (request: InitiateRequestValidatedFields)
        : Validation<ShortNameWithCipheringInitiateRequest> =
        validator {
            let! conformance =
                ShortNameConformance.fromConformance request.ProposedConformance

            return {
                DedicatedKey = request.DedicatedKey
                ResponseAllowed = request.ResponseAllowed
                ProposedDlmsVersionNumber = request.ProposedDlmsVersionNumber
                ProposedConformance = conformance
                ClientMaxReceivePduSize = request.ClientMaxReceivePduSize
            }
        }

type private CipheredAarqClientFields =
    {
        ClientSystemTitle : ClientSystemTitle option
        ClientDigitalSignatureCertificate : ClientDigitalSignatureCertificate option
        ClientUserIdentifier : ClientUserIdentifier option
    }

module private CipheredAarqClientFields =
    let fromAarqFields (aarq: AarqValidatedFields) : Validation<CipheredAarqClientFields> =
        validator {
            let! clientSystemTitle =
                aarq.CallingApTitle
                |> Validation.traverseOption ClientSystemTitle.fromApTitle

            and! clientCertificate =
                aarq.CallingAeQualifier
                |> Validation.traverseOption ClientDigitalSignatureCertificate.fromAeQualifier

            let clientUserIdentifier =
                aarq.CallingAeInvocationId
                |> Option.map ClientUserIdentifier.fromInvocationIdentifier

            return {
                ClientSystemTitle = clientSystemTitle
                ClientDigitalSignatureCertificate = clientCertificate
                ClientUserIdentifier = clientUserIdentifier
            }
        }

type LogicalNameWithCipheringOpenRequest =
    {
        ClientSystemTitle : ClientSystemTitle option
        ClientDigitalSignatureCertificate : ClientDigitalSignatureCertificate option
        ClientUserIdentifier : ClientUserIdentifier option
        Authentication : Authentication
        InitiateRequest : LogicalNameWithCipheringInitiateRequest
    }

module LogicalNameWithCipheringOpenRequest =
    let fromFields (aarq: AarqValidatedFields) (initiate: InitiateRequestValidatedFields) : Validation<
                                                                                                LogicalNameWithCipheringOpenRequest
                                                                                                > =
        validator {
            let! initiate =
                LogicalNameWithCipheringInitiateRequest.fromInitiateRequest initiate

            and! clientIdentity =
                CipheredAarqClientFields.fromAarqFields aarq

            return {
                ClientSystemTitle = clientIdentity.ClientSystemTitle
                ClientDigitalSignatureCertificate = clientIdentity.ClientDigitalSignatureCertificate
                ClientUserIdentifier = clientIdentity.ClientUserIdentifier
                Authentication = aarq.Authentication
                InitiateRequest = initiate
            }
        }

type ShortNameNoCipheringOpenRequest =
    {
        ClientUserIdentifier : ClientUserIdentifier option
        Authentication : Authentication
        InitiateRequest : ShortNameNoCipheringInitiateRequest
    }

module ShortNameNoCipheringOpenRequest =
    let fromFields
        (aarq: AarqValidatedFields)
        (initiate: InitiateRequestValidatedFields)
        : Validation<ShortNameNoCipheringOpenRequest> =
        validator {
            let! initiate =
                ShortNameNoCipheringInitiateRequest.fromInitiateRequest initiate

            let userId =
                aarq.CallingAeInvocationId
                |> Option.map ClientUserIdentifier.fromInvocationIdentifier

            return {
                ClientUserIdentifier = userId
                Authentication = aarq.Authentication
                InitiateRequest = initiate
            }
        }

type ShortNameWithCipheringOpenRequest =
    {
        ClientSystemTitle : ClientSystemTitle option
        ClientDigitalSignatureCertificate : ClientDigitalSignatureCertificate option
        ClientUserIdentifier : ClientUserIdentifier option
        Authentication : Authentication
        InitiateRequest : ShortNameWithCipheringInitiateRequest
    }

module ShortNameWithCipheringOpenRequest =
    let fromFields (aarq: AarqValidatedFields) (initiate: InitiateRequestValidatedFields) : Validation<
                                                                                                ShortNameWithCipheringOpenRequest
                                                                                                > =
        validator {
            let! initiate =
                ShortNameWithCipheringInitiateRequest.fromInitiateRequest initiate

            and! clientIdentity =
                CipheredAarqClientFields.fromAarqFields aarq

            return {
                ClientSystemTitle = clientIdentity.ClientSystemTitle
                ClientDigitalSignatureCertificate = clientIdentity.ClientDigitalSignatureCertificate
                ClientUserIdentifier = clientIdentity.ClientUserIdentifier
                Authentication = aarq.Authentication
                InitiateRequest = initiate
            }
        }

type OpenRequest =
    | LogicalNameNoCiphering of LogicalNameNoCipheringOpenRequest
    | LogicalNameWithCiphering of LogicalNameWithCipheringOpenRequest
    | ShortNameNoCiphering of ShortNameNoCipheringOpenRequest
    | ShortNameWithCiphering of ShortNameWithCipheringOpenRequest

module OpenRequest =
    let fromFields (aarqFields: AarqValidatedFields) (initiateRequest: InitiateRequestValidatedFields) : Validation<OpenRequest> =
        validator {
            match aarqFields.ApplicationContextName with
            | ApplicationContextName.LogicalNameNoCiphering ->
                return!
                    LogicalNameNoCipheringOpenRequest.fromFields aarqFields initiateRequest
                    |> Validation.map LogicalNameNoCiphering

            | ApplicationContextName.LogicalNameWithCiphering ->
                return!
                    LogicalNameWithCipheringOpenRequest.fromFields aarqFields initiateRequest
                    |> Validation.map LogicalNameWithCiphering

            | ApplicationContextName.ShortNameNoCiphering ->
                return!
                    ShortNameNoCipheringOpenRequest.fromFields aarqFields initiateRequest
                    |> Validation.map ShortNameNoCiphering

            | ApplicationContextName.ShortNameWithCiphering ->
                return!
                    ShortNameWithCipheringOpenRequest.fromFields aarqFields initiateRequest
                    |> Validation.map ShortNameWithCiphering
        }

    let fromRaw (raw: OpenRequestRawFields) : Validation<OpenRequest> =
        match raw.UserInformation with
        | XdlmsApduRaw.InitiateRequest initiateRaw ->
                validator {
                    let! initiateRequest =
                        InitiateRequestValidatedFields.fromRaw initiateRaw

                    and! aarq =
                        AarqValidatedFields.fromRaw raw.Aarq

                    return! fromFields aarq initiateRequest
                }

        | _ ->
            validationError
                ["AARQ"; "user-information"]
                "encrypted AARQ is not supported yet"