namespace Metering.Mbus.Analysis

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.AuthenticationAndFragmentationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.ExtendedLinkLayer
open Metering.Mbus.Protocol.Frames.NetworkLayer
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Mbus.Protocol.Messages

module MbusAnalyzer =

    /// None means absent; a present ELL retains its validation outcome separately.
    type private EllAnalysis = {
        Validation: AnalysisOperation<Field<Ell>> option
        Following: AnalysisInput
    }

    let private assembleFrame
        (dll: Field<Dll> option)
        (data: LinkUserData option)
        : Field<WiredMbusFrame> option =
        dll
        |> Option.bind (fun dll ->
            match dll.Value, data with
            | Dll.SingleCharacter, _ ->
                Some(Field.withValue (WiredMbusFrame.SingleCharacter(Field.withValue () dll)) dll)
            | Dll.FixedLength frame, _ ->
                Some(Field.withValue (WiredMbusFrame.FixedLength(Field.withValue frame dll)) dll)
            | Dll.VariableLength frame, Some data ->
                Some(
                    Field.withValue
                        (WiredMbusFrame.VariableLength(Field.withValue frame dll, data))
                        dll
                )
            | Dll.VariableLength _, None -> None)

    let private assembleCompleteFrame
        (session: AnalysisSession)
        (dll: AnalysisOperation<Field<Dll>>)
        (ell: EllAnalysis)
        (raw: Field<TplRaw>)
        (tpl: AnalysisOperation<Field<Tpl>>)
        (apl: AnalysisOperation<Field<Apl>>)
        : Field<WiredMbusFrame> option =
        match tpl, apl with
        | AnalysisOperation.Succeeded(tpl, _), AnalysisOperation.Succeeded(apl, _) when
            not session.HasFailures
            ->
            let complete: CompleteLinkUserData = {
                Ell = ell.Validation |> Option.bind _.Value
                Nwl = None
                Afl = None
                Tpl = tpl
                Apl = apl
            }

            let higher = Field.withValue complete raw
            let data = LinkUserData.MbusProtocol(MbusProtocolUserData.Complete higher)
            assembleFrame dll.Value (Some data)
        | _ -> None

    let private finishMessage
        (session: AnalysisSession)
        (dependencies: AnalysisStepId list)
        (frame: Field<WiredMbusFrame> option)
        : unit =
        match frame with
        | Some frame ->
            session.Validate MbusAnalysisStep.MessageValidation dependencies frame Message.validate
            |> ignore
        | None ->
            session.Block
                MbusAnalysisStep.MessageValidation
                session.Input.Span
                session.FirstFailureId
                "A fully validated frame is required for message mapping."

    let private analyzeSelection
        (session: AnalysisSession)
        (dll: AnalysisOperation<Field<Dll>>)
        (input: AnalysisInput)
        : unit =
        match session.Parse MbusAnalysisStep.SelectionParsing input SelectionCodec.parse with
        | AnalysisOperation.Failed parseStep ->
            session.Block
                MbusAnalysisStep.MessageValidation
                input.Span
                parseStep
                "Selection parsing failed."
        | AnalysisOperation.Succeeded(raw, parseStep) ->
            let selection =
                session.Validate
                    MbusAnalysisStep.SelectionValidation
                    [ parseStep ]
                    raw
                    SelectionCodec.validate

            let data =
                selection.Value
                |> Option.map (fun selection -> LinkUserData.SelectionOfDevice selection.Value)

            finishMessage session [ dll.StepId; selection.StepId ] (assembleFrame dll.Value data)

    let private unprotectApplication
        (session: AnalysisSession)
        (resolver: IExternalSecurityContextResolver)
        (raw: Field<TplRaw>)
        (parseStep: AnalysisStepId)
        : AnalysisInput option =
        let payload = TplRaw.aplData raw.Value

        let protection =
            session.Execute
                MbusAnalysisStep.ProtectionPreparation
                [ parseStep ]
                payload.Span
                (MbusProtectionInput.decodeRaw raw)

        match protection with
        | AnalysisOperation.Failed prepareStep ->
            session.Block
                MbusAnalysisStep.AplParsing
                payload.Span
                prepareStep
                "Protection prerequisites are not satisfied."

            None
        | AnalysisOperation.Succeeded(protection, prepareStep) ->
            let unprotection =
                session.Execute
                    MbusAnalysisStep.ApplicationUnprotection
                    [ prepareStep ]
                    payload.Span
                    (MbusProtection.decode resolver protection)

            match unprotection with
            | AnalysisOperation.Failed unprotectStep ->
                session.Block
                    MbusAnalysisStep.AplParsing
                    payload.Span
                    unprotectStep
                    "Application bytes could not be unprotected."

                None
            | AnalysisOperation.Succeeded(plain, unprotectStep) ->
                Some {
                    Span = plain.Span
                    Dependencies = [ unprotectStep ]
                }

    let private analyzeApplication
        (session: AnalysisSession)
        (ci: CiFieldTpl)
        (input: AnalysisInput)
        : AnalysisOperation<Field<Apl>> option =
        match session.Parse MbusAnalysisStep.AplParsing input (AplCodec.parse ci) with
        | AnalysisOperation.Failed parseStep ->
            session.Block
                MbusAnalysisStep.MessageValidation
                input.Span
                parseStep
                "Application parsing failed."

            None
        | AnalysisOperation.Succeeded(raw, parseStep) ->
            Some(
                session.Validate MbusAnalysisStep.AplValidation [ parseStep ] raw AplCodec.validate
            )

    let private analyzeTransport
        (session: AnalysisSession)
        (resolver: IExternalSecurityContextResolver)
        (dll: AnalysisOperation<Field<Dll>>)
        (ell: EllAnalysis)
        (input: AnalysisInput)
        : unit =
        match session.Parse MbusAnalysisStep.TplParsing input TplCodec.parse with
        | AnalysisOperation.Failed parseStep ->
            session.Block
                MbusAnalysisStep.AplParsing
                input.Span
                parseStep
                "TPL parsing did not provide application boundaries and protection metadata."
        | AnalysisOperation.Succeeded(raw, parseStep) ->
            let tpl =
                session.Validate MbusAnalysisStep.TplValidation [ parseStep ] raw TplCodec.validate
            // Continuation depends on concrete protection prerequisites, not whole-TPL validity.
            let application =
                unprotectApplication session resolver raw parseStep
                |> Option.bind (analyzeApplication session (TplRaw.ci raw.Value).Value)

            match application with
            | None -> () // The operation that stopped continuation already recorded its blocker.
            | Some apl ->
                let frame = assembleCompleteFrame session dll ell raw tpl apl

                let ellSteps =
                    ell.Validation
                    |> Option.map (fun result -> [ result.StepId ])
                    |> Option.defaultValue []

                finishMessage session ([ dll.StepId; tpl.StepId; apl.StepId ] @ ellSteps) frame

    let private analyzeEll (session: AnalysisSession) (input: AnalysisInput) : EllAnalysis option =
        match session.ParseOptional MbusAnalysisStep.EllParsing input EllCodec.tryParse with
        | OptionalParse.Failed parseStep ->
            session.Block MbusAnalysisStep.HigherLayers input.Span parseStep "ELL parsing failed."
            None
        | OptionalParse.Absent following ->
            Some {
                Validation = None
                Following = following
            }
        | OptionalParse.Present(raw, following) ->
            let validation =
                session.Validate
                    MbusAnalysisStep.EllValidation
                    following.Dependencies
                    raw
                    EllCodec.validate

            Some {
                Validation = Some validation
                Following = following
            }

    let private rejectUnsupportedLayer
        (session: AnalysisSession)
        (name: string)
        (parser: Parser<Field<'raw> option>)
        (validator: Field<'raw> -> Validation<Field<'value>>)
        (parseStep: AnalysisStep<unit> -> MbusAnalysisStep)
        (validationStep: AnalysisStep<Field<'value>> -> MbusAnalysisStep)
        (input: AnalysisInput)
        : AnalysisInput option =
        match session.ParseOptional parseStep input parser with
        | OptionalParse.Failed parseStep ->
            session.Block
                MbusAnalysisStep.TplParsing
                input.Span
                parseStep
                (name + " does not provide a supported following payload.")

            None
        | OptionalParse.Absent following -> Some following
        | OptionalParse.Present(raw, following) ->
            let validation =
                session.Validate validationStep following.Dependencies raw validator

            session.Block
                MbusAnalysisStep.TplParsing
                input.Span
                validation.StepId
                (name + " payload processing is not implemented.")

            None

    let private analyzeHigherLayers
        (session: AnalysisSession)
        (resolver: IExternalSecurityContextResolver)
        (dll: AnalysisOperation<Field<Dll>>)
        (input: AnalysisInput)
        : unit =
        match analyzeEll session input with
        | None -> ()
        | Some ell ->
            ell.Following
            |> rejectUnsupportedLayer
                session
                "NWL"
                NwlCodec.tryParse
                NwlCodec.validate
                MbusAnalysisStep.NwlParsing
                MbusAnalysisStep.NwlValidation
            |> Option.bind (
                rejectUnsupportedLayer
                    session
                    "AFL"
                    AflCodec.tryParse
                    AflCodec.validate
                    MbusAnalysisStep.AflParsing
                    MbusAnalysisStep.AflValidation
            )
            |> Option.iter (analyzeTransport session resolver dll ell)

    let private analyzeLinkUserData
        (session: AnalysisSession)
        (resolver: IExternalSecurityContextResolver)
        (dll: AnalysisOperation<Field<Dll>>)
        (input: AnalysisInput)
        : unit =
        let ciParser =
            parser {
                let! ci = CiField.peek
                let! _ = takeAll
                return ci
            }

        match session.Parse MbusAnalysisStep.CiParsing input ciParser with
        | AnalysisOperation.Failed parseStep ->
            session.Block
                MbusAnalysisStep.LinkUserData
                input.Span
                parseStep
                "The layer discriminator is not known."
        | AnalysisOperation.Succeeded(ci, parseStep) ->
            let following = {
                input with
                    Dependencies = [ parseStep ]
            }

            match ci with
            | CiField.LowerLayerManagement CiLowerLayerManagement.SelectionOfDevice ->
                analyzeSelection session dll following
            | _ -> analyzeHigherLayers session resolver dll following

    let private analyzeFrame
        (session: AnalysisSession)
        (resolver: IExternalSecurityContextResolver)
        : unit =
        let input = {
            Span = session.Input.Span
            Dependencies = []
        }

        match session.Parse MbusAnalysisStep.DllParsing input DllCodec.parse with
        | AnalysisOperation.Failed parseStep ->
            session.Block
                MbusAnalysisStep.LinkUserData
                input.Span
                parseStep
                "DLL parsing did not provide bounded Link User Data."
        | AnalysisOperation.Succeeded(raw, parseStep) ->
            let dll =
                session.Validate MbusAnalysisStep.DllValidation [ parseStep ] raw DllCodec.validate

            TelegramType.tryIdentify raw
            |> Option.iter (fun identity ->
                session.Execute
                    MbusAnalysisStep.TelegramIdentification
                    [ parseStep ]
                    identity.Span
                    (Decoder.decodePassed identity)
                |> ignore)

            match raw.Value with
            | DllRaw.SingleCharacter
            | DllRaw.FixedLength _ ->
                finishMessage session [ dll.StepId ] (assembleFrame dll.Value None)
            | DllRaw.VariableLength frame ->
                let following = {
                    Span = frame.UserData.Value.LinkUserData.Span
                    Dependencies = [ parseStep ]
                }

                analyzeLinkUserData session resolver dll following

    /// Analyzes one bounded wired frame. Each call owns its trace and source store.
    let analyze
        (resolver: IExternalSecurityContextResolver)
        (bytes: ReadOnlyMemory<byte>)
        : MbusAnalysis =
        let session = AnalysisSession bytes
        analyzeFrame session resolver
        session.Snapshot()
