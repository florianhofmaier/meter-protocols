namespace Metering.Dlms.Protocol.Xdlms

open Metering.Common.Parsers.BinaryParsers
open Metering.Common.Parsers.Core

type ServiceErrorRaw =
    | ApplicationReference of byte
    | HardwareResource of byte
    | VdeStateError of byte
    | Service of byte
    | Definition of byte
    | Access of byte
    | Initiate of byte
    | LoadDataSet of byte
    | Task of byte

module ServiceErrorRaw =
    type Tag =
        | ApplicationReference = 0x00uy
        | HardwareResource = 0x01uy
        | VdeStateError = 0x02uy
        | Service = 0x03uy
        | Definition = 0x04uy
        | Access = 0x05uy
        | Initiate = 0x06uy
        | LoadDataSet = 0x07uy
        | Task = 0x09uy

    let parse : Parser<ServiceErrorRaw> =
        withCtx "ServiceError" <|
        parser {
            let! tag = Tag.parse<Tag>
            let! value = parseU8

            match tag with
            | Tag.ApplicationReference ->
                return ApplicationReference value

            | Tag.HardwareResource ->
                return HardwareResource value

            | Tag.VdeStateError ->
                return VdeStateError value

            | Tag.Service ->
                return Service value

            | Tag.Definition ->
                return Definition value

            | Tag.Access ->
                return Access value

            | Tag.Initiate ->
                return Initiate value

            | Tag.LoadDataSet ->
                return LoadDataSet value

            | Tag.Task ->
                return Task value

            | _ ->
                return! fail $"unsupported ServiceError tag {tag}"
        }

type ConfirmedServiceErrorRaw =
    | InitiateError of ServiceErrorRaw
    | Read of ServiceErrorRaw
    | Write of ServiceErrorRaw

module ConfirmedServiceErrorRaw =
    type ChoiceTag =
        | InitiateError = 0x01uy
        | Read = 0x05uy
        | Write = 0x06uy

    let parseBody : Parser<ConfirmedServiceErrorRaw> =
        parser {
            let! tag = Tag.parse<ChoiceTag>

            match tag with
            | ChoiceTag.InitiateError ->
                return! ServiceErrorRaw.parse |>> InitiateError

            | ChoiceTag.Read ->
                return! ServiceErrorRaw.parse |>> Read

            | ChoiceTag.Write ->
                return! ServiceErrorRaw.parse |>> Write

            | _ ->
                return! fail $"unsupported ConfirmedServiceError choice {tag}"
        }

type ApplicationReferenceError =
    | Other
    | TimeElapsed
    | ApplicationUnreachable
    | ApplicationReferenceInvalid
    | ApplicationContextUnsupported
    | ProviderCommunicationError
    | DecipheringError

module ApplicationReferenceError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk TimeElapsed
        | 2uy ->validationOk ApplicationUnreachable
        | 3uy ->validationOk ApplicationReferenceInvalid
        | 4uy ->validationOk ApplicationContextUnsupported
        | 5uy ->validationOk ProviderCommunicationError
        | 6uy ->validationOk DecipheringError
        | other ->
            validationError
                $"unsupported application-reference error {other}"

type HardwareResourceError =
    | Other
    | MemoryUnavailable
    | ProcessorResourceUnavailable
    | MassStorageUnavailable
    | OtherResourceUnavailable

module HardwareResourceError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk MemoryUnavailable
        | 2uy ->validationOk ProcessorResourceUnavailable
        | 3uy ->validationOk MassStorageUnavailable
        | 4uy ->validationOk OtherResourceUnavailable
        | other ->
            validationError
                $"unsupported hardware-resource error {other}"

type VdeStateError =
    | Other
    | NoDlmsContext
    | LoadingDataSet
    | StatusNoChange
    | StatusInoperable

module VdeStateError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk NoDlmsContext
        | 2uy ->validationOk LoadingDataSet
        | 3uy ->validationOk StatusNoChange
        | 4uy ->validationOk StatusInoperable
        | other ->
            validationError
                $"unsupported vde-state-error {other}"

type ServiceError =
    | Other
    | PduSize
    | ServiceUnsupported

module ServiceError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk PduSize
        | 2uy ->validationOk ServiceUnsupported
        | other ->
            validationError
                $"unsupported service error {other}"

type DefinitionError =
    | Other
    | ObjectUndefined
    | ObjectClassInconsistent
    | ObjectAttributeInconsistent

module DefinitionError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk ObjectUndefined
        | 2uy ->validationOk ObjectClassInconsistent
        | 3uy ->validationOk ObjectAttributeInconsistent
        | other ->
            validationError
                $"unsupported definition error {other}"

type AccessError =
    | Other
    | ScopeOfAccessViolated
    | ObjectAccessViolated
    | HardwareFault
    | ObjectUnavailable

module AccessError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk ScopeOfAccessViolated
        | 2uy ->validationOk ObjectAccessViolated
        | 3uy ->validationOk HardwareFault
        | 4uy ->validationOk ObjectUnavailable
        | other ->
            validationError
                $"unsupported access error {other}"

type InitiateError =
    | Other
    | DlmsVersionTooLow
    | IncompatibleConformance
    | PduSizeTooShort
    | RefusedByTheVdeHandler

module InitiateError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk DlmsVersionTooLow
        | 2uy ->validationOk IncompatibleConformance
        | 3uy ->validationOk PduSizeTooShort
        | 4uy ->validationOk RefusedByTheVdeHandler
        | other ->
            validationError
                $"unsupported initiate error {other}"

type LoadDataSetError =
    | Other
    | PrimitiveOutOfSequence
    | NotLoadable
    | DatasetSizeTooLarge
    | NotAwaitedSegment
    | InterpretationFailure
    | StorageFailure
    | DataSetNotReady

module LoadDataSetError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk PrimitiveOutOfSequence
        | 2uy ->validationOk NotLoadable
        | 3uy ->validationOk DatasetSizeTooLarge
        | 4uy ->validationOk NotAwaitedSegment
        | 5uy ->validationOk InterpretationFailure
        | 6uy ->validationOk StorageFailure
        | 7uy ->validationOk DataSetNotReady
        | other ->
            validationError
                $"unsupported load-data-set error {other}"

type TaskError =
    | Other
    | NoRemoteControl
    | TiStopped
    | TiRunning
    | TiUnusable

module TaskError =
    let fromRaw value =
        match value with
        | 0uy ->validationOk Other
        | 1uy ->validationOk NoRemoteControl
        | 2uy ->validationOk TiStopped
        | 3uy ->validationOk TiRunning
        | 4uy ->validationOk TiUnusable
        | other ->
            validationError
                $"unsupported task error {other}"

type ServiceErrorChoice =
    | ApplicationReference of ApplicationReferenceError
    | HardwareResource of HardwareResourceError
    | VdeStateError of VdeStateError
    | Service of ServiceError
    | Definition of DefinitionError
    | Access of AccessError
    | Initiate of InitiateError
    | LoadDataSet of LoadDataSetError
    | Task of TaskError

module ServiceErrorChoice =
    let fromRaw raw : Validation<ServiceErrorChoice> =
        match raw with
        | ServiceErrorRaw.ApplicationReference value ->
            value
            |> ApplicationReferenceError.fromRaw
            |> Validation.map ServiceErrorChoice.ApplicationReference

        | ServiceErrorRaw.HardwareResource value ->
            value
            |> HardwareResourceError.fromRaw
            |> Validation.map ServiceErrorChoice.HardwareResource

        | ServiceErrorRaw.VdeStateError value ->
            value
            |> VdeStateError.fromRaw
            |> Validation.map ServiceErrorChoice.VdeStateError

        | ServiceErrorRaw.Service value ->
            value
            |> ServiceError.fromRaw
            |> Validation.map ServiceErrorChoice.Service

        | ServiceErrorRaw.Definition value ->
            value
            |> DefinitionError.fromRaw
            |> Validation.map ServiceErrorChoice.Definition

        | ServiceErrorRaw.Access value ->
            value
            |> AccessError.fromRaw
            |> Validation.map ServiceErrorChoice.Access

        | ServiceErrorRaw.Initiate value ->
            value
            |> InitiateError.fromRaw
            |> Validation.map ServiceErrorChoice.Initiate

        | ServiceErrorRaw.LoadDataSet value ->
            value
            |> LoadDataSetError.fromRaw
            |> Validation.map ServiceErrorChoice.LoadDataSet

        | ServiceErrorRaw.Task value ->
            value
            |> TaskError.fromRaw
            |> Validation.map ServiceErrorChoice.Task

type ConfirmedServiceError =
    | InitiateError of ServiceErrorChoice
    | Read of ServiceErrorChoice
    | Write of ServiceErrorChoice

module ConfirmedServiceError =
    let fromRaw raw : Validation<ConfirmedServiceError> =
        match raw with
        | ConfirmedServiceErrorRaw.InitiateError error ->
            error
            |> ServiceErrorChoice.fromRaw
            |> Validation.map ConfirmedServiceError.InitiateError

        | ConfirmedServiceErrorRaw.Read error ->
            error
            |> ServiceErrorChoice.fromRaw
            |> Validation.map ConfirmedServiceError.Read

        | ConfirmedServiceErrorRaw.Write error ->
            error
            |> ServiceErrorChoice.fromRaw
            |> Validation.map ConfirmedServiceError.Write