namespace Metering.Dlms.Protocol.Xdlms

open Metering.Common.Parsers.BinaryParsers
open Metering.Common.Parsers.Core
open Metering.Dlms.Protocol

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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok TimeElapsed
        | 2uy -> Validation.ok ApplicationUnreachable
        | 3uy -> Validation.ok ApplicationReferenceInvalid
        | 4uy -> Validation.ok ApplicationContextUnsupported
        | 5uy -> Validation.ok ProviderCommunicationError
        | 6uy -> Validation.ok DecipheringError
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok MemoryUnavailable
        | 2uy -> Validation.ok ProcessorResourceUnavailable
        | 3uy -> Validation.ok MassStorageUnavailable
        | 4uy -> Validation.ok OtherResourceUnavailable
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok NoDlmsContext
        | 2uy -> Validation.ok LoadingDataSet
        | 3uy -> Validation.ok StatusNoChange
        | 4uy -> Validation.ok StatusInoperable
        | other ->
            Validation.error
                $"unsupported vde-state-error {other}"

type ServiceError =
    | Other
    | PduSize
    | ServiceUnsupported

module ServiceError =
    let fromRaw value =
        match value with
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok PduSize
        | 2uy -> Validation.ok ServiceUnsupported
        | other ->
            Validation.error
                $"unsupported service error {other}"

type DefinitionError =
    | Other
    | ObjectUndefined
    | ObjectClassInconsistent
    | ObjectAttributeInconsistent

module DefinitionError =
    let fromRaw value =
        match value with
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok ObjectUndefined
        | 2uy -> Validation.ok ObjectClassInconsistent
        | 3uy -> Validation.ok ObjectAttributeInconsistent
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok ScopeOfAccessViolated
        | 2uy -> Validation.ok ObjectAccessViolated
        | 3uy -> Validation.ok HardwareFault
        | 4uy -> Validation.ok ObjectUnavailable
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok DlmsVersionTooLow
        | 2uy -> Validation.ok IncompatibleConformance
        | 3uy -> Validation.ok PduSizeTooShort
        | 4uy -> Validation.ok RefusedByTheVdeHandler
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok PrimitiveOutOfSequence
        | 2uy -> Validation.ok NotLoadable
        | 3uy -> Validation.ok DatasetSizeTooLarge
        | 4uy -> Validation.ok NotAwaitedSegment
        | 5uy -> Validation.ok InterpretationFailure
        | 6uy -> Validation.ok StorageFailure
        | 7uy -> Validation.ok DataSetNotReady
        | other ->
            Validation.error
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
        | 0uy -> Validation.ok Other
        | 1uy -> Validation.ok NoRemoteControl
        | 2uy -> Validation.ok TiStopped
        | 3uy -> Validation.ok TiRunning
        | 4uy -> Validation.ok TiUnusable
        | other ->
            Validation.error
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