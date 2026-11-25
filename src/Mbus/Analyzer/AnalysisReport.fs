namespace Metering.Mbus.Analysis

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages

/// An analysis operation identity, distinct from field and source identities.
[<Struct>]
type AnalysisStepId =
    private
    | AnalysisStepId of int

    member this.Value: int =
        let (AnalysisStepId value) = this
        value

    static member internal Create(value: int) : AnalysisStepId = AnalysisStepId value

/// The outcome of a concrete operation; successful values cannot be absent.
[<RequireQualifiedAccess>]
type AnalysisStepResult<'value> =
    | Succeeded of 'value
    | Failed of DecodeFailure
    | Blocked of cause: AnalysisStepId * reason: string

/// Value-independent view for navigation, diagnostics and overall validity.
[<RequireQualifiedAccess>]
type AnalysisStepStatus =
    | Succeeded
    | Failed of DecodeFailure
    | Blocked of cause: AnalysisStepId * reason: string

type AnalysisStepMetadata = {
    Id: AnalysisStepId
    Input: SourceSpan
    Dependencies: AnalysisStepId list
    Notices: Notice list
}

/// Common observation of heterogeneous, concretely typed steps.
type IAnalysisStep =
    abstract Metadata: AnalysisStepMetadata
    abstract Status: AnalysisStepStatus

/// One operation, with its result type fixed by the enclosing M-Bus step variant.
type AnalysisStep<'value> = {
    Metadata: AnalysisStepMetadata
    Result: AnalysisStepResult<'value>
} with

    interface IAnalysisStep with
        member step.Metadata = step.Metadata

        member step.Status =
            match step.Result with
            | AnalysisStepResult.Succeeded _ -> AnalysisStepStatus.Succeeded
            | AnalysisStepResult.Failed error -> AnalysisStepStatus.Failed error
            | AnalysisStepResult.Blocked(cause, reason) -> AnalysisStepStatus.Blocked(cause, reason)

/// Closed protocol operations: every variant fixes its own successful result type.
/// Parsing is observed through the field trace, without retaining another raw-value collection.
[<RequireQualifiedAccess>]
type MbusAnalysisStep =
    | DllParsing of AnalysisStep<unit>
    | DllValidation of AnalysisStep<Field<Dll>>
    | TelegramIdentification of AnalysisStep<Field<TelegramType>>
    | CiParsing of AnalysisStep<unit>
    | SelectionParsing of AnalysisStep<unit>
    | SelectionValidation of AnalysisStep<Field<DeviceSelection>>
    | EllParsing of AnalysisStep<unit>
    | EllValidation of AnalysisStep<Field<Ell>>
    | NwlParsing of AnalysisStep<unit>
    | NwlValidation of AnalysisStep<Field<Nwl>>
    | AflParsing of AnalysisStep<unit>
    | AflValidation of AnalysisStep<Field<Afl>>
    | TplParsing of AnalysisStep<unit>
    | TplValidation of AnalysisStep<Field<Tpl>>
    | ProtectionPreparation of AnalysisStep<MbusProtectionInput>
    | ApplicationUnprotection of AnalysisStep<Field<ReadOnlyMemory<byte>>>
    | AplParsing of AnalysisStep<unit>
    | AplValidation of AnalysisStep<Field<Apl>>
    | MessageValidation of AnalysisStep<Field<Message>>
    | LinkUserData of AnalysisStep<unit>
    | HigherLayers of AnalysisStep<unit>

    member private this.Details: IAnalysisStep =
        match this with
        | MbusAnalysisStep.DllParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.DllValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.TelegramIdentification step -> step :> IAnalysisStep
        | MbusAnalysisStep.CiParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.SelectionParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.SelectionValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.EllParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.EllValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.NwlParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.NwlValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.AflParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.AflValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.TplParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.TplValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.ProtectionPreparation step -> step :> IAnalysisStep
        | MbusAnalysisStep.ApplicationUnprotection step -> step :> IAnalysisStep
        | MbusAnalysisStep.AplParsing step -> step :> IAnalysisStep
        | MbusAnalysisStep.AplValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.MessageValidation step -> step :> IAnalysisStep
        | MbusAnalysisStep.LinkUserData step -> step :> IAnalysisStep
        | MbusAnalysisStep.HigherLayers step -> step :> IAnalysisStep

    member this.Metadata = this.Details.Metadata
    member this.Status = this.Details.Status

    /// Stable display/export name derived from the operation, never supplied independently.
    member this.Name =
        match this with
        | MbusAnalysisStep.DllParsing _ -> "DLL.parse"
        | MbusAnalysisStep.DllValidation _ -> "DLL.validate"
        | MbusAnalysisStep.TelegramIdentification _ -> "Telegram.identify"
        | MbusAnalysisStep.CiParsing _ -> "CI.parse"
        | MbusAnalysisStep.SelectionParsing _ -> "Selection.parse"
        | MbusAnalysisStep.SelectionValidation _ -> "Selection.validate"
        | MbusAnalysisStep.EllParsing _ -> "ELL.parse"
        | MbusAnalysisStep.EllValidation _ -> "ELL.validate"
        | MbusAnalysisStep.NwlParsing _ -> "NWL.parse"
        | MbusAnalysisStep.NwlValidation _ -> "NWL.validate"
        | MbusAnalysisStep.AflParsing _ -> "AFL.parse"
        | MbusAnalysisStep.AflValidation _ -> "AFL.validate"
        | MbusAnalysisStep.TplParsing _ -> "TPL.parse"
        | MbusAnalysisStep.TplValidation _ -> "TPL.validate"
        | MbusAnalysisStep.ProtectionPreparation _ -> "Protection.prepare"
        | MbusAnalysisStep.ApplicationUnprotection _ -> "Protection.unprotect"
        | MbusAnalysisStep.AplParsing _ -> "APL.parse"
        | MbusAnalysisStep.AplValidation _ -> "APL.validate"
        | MbusAnalysisStep.MessageValidation _ -> "Message.validate"
        | MbusAnalysisStep.LinkUserData _ -> "LinkUserData"
        | MbusAnalysisStep.HigherLayers _ -> "HigherLayers"

/// A source and its bytes as report data; no runtime source-store service is retained.
type AnalysisSource = {
    Info: SourceInfo
    Bytes: ReadOnlyMemory<byte>
}

/// Complete typed report. Raw bytes remain referenced by field/source spans.
type MbusAnalysis = {
    Trace: ParseTrace
    Steps: MbusAnalysisStep list
    Sources: AnalysisSource list
} with

    member report.IsValid =
        report.Steps
        |> List.forall (fun step ->
            match step.Status with
            | AnalysisStepStatus.Succeeded -> true
            | _ -> false)
