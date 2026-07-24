namespace Metering.Mbus.Protocol.Frames

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.Application
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security

type CompleteMessageRaw =
    {
        Dll: Field<DllVariableLengthRaw>
        Tpl: Field<TplRaw>
    }

type FrameVariableLengthRaw =
    | CompleteMessage of CompleteMessageRaw

type ProtectedAplRaw =
    {
        OriginalPayload: Field<ReadOnlyMemory<byte>>
        EncryptedPart: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
        Failure: UnprotectionFailure
    }

type AplExpansionRaw =
    | Parsed of Field<AplRaw>
    | Protected of ProtectedAplRaw
    | Invalid of Failures

type CompleteMessageExpandedRaw =
    {
        Dll: Field<DllVariableLengthRaw>
        Tpl: Field<TplRaw>
        Payload: AplExpansionRaw
    }

type FrameVariableLengthExpandedRaw =
    | CompleteMessage of CompleteMessageExpandedRaw

type ProtectedApl =
    {
        OriginalPayload: Field<ReadOnlyMemory<byte>>
        EncryptedPart: Field<ReadOnlyMemory<byte>>
        ClearSuffix: Field<ReadOnlyMemory<byte>> option
        Failure: UnprotectionFailure
    }

type AplContent =
    | Decoded of Field<Apl>
    | Protected of ProtectedApl

type CompleteMessage =
    {
        Dll: Field<DllVariableLength>
        Tpl: Field<Tpl>
        Payload: AplContent
    }

type FrameVariableLength =
    | CompleteMessage of CompleteMessage

module private PipelineIssues =

    let single (field: Field<_>) message =
        Failures.single {
            FieldId = field.Id
            Message = message
        }

module FrameVariableLengthRaw =

    let parseCompleteMessageFromDll
        (dll: Field<DllVariableLengthRaw>)
        : Decoder<Field<FrameVariableLengthRaw>> =

        let source =
            dll.Value.UserData.Value.HigherLayerData

        if source.Value.Length > 0 && source.Value.Span[0] = 0x90uy then
            decodeError (
                ParseFailed {
                    Source = source.Span.Source
                    Pos = source.Span.Offset
                    Msg =
                        "Unsupported but standard-conformant AFL variant: CI=0x90 at HigherLayerData; "
                        + "supported path is a complete TPL message. EN 13757-7:2018, 5.2, Table 2."
                }
            )
        else
            decoder {
                let! tpl =
                    parse TplRaw.parse source

                return
                    dll
                    |> Field.withValue (
                        FrameVariableLengthRaw.CompleteMessage {
                            Dll = dll
                            Tpl = tpl
                        }
                    )
            }

module private AplParser =

    let capture
        ci
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Result<Field<AplRaw>, Failures>> =

        fun context ->
            match parse (AplRaw.parse ci) source context with
            | Metering.Common.Decoding.Decoders.Core.Decoded (value, notices) ->
                Metering.Common.Decoding.Decoders.Core.Decoded (Ok value, notices)

            | DecodeFailed (ParseFailed error, notices) ->
                let failures =
                    PipelineIssues.single
                        source
                        $"Invalid unprotected APL at byte {error.Pos}: {error.Msg}"

                Metering.Common.Decoding.Decoders.Core.Decoded (Error failures, notices)

            | DecodeFailed (failure, notices) ->
                DecodeFailed (failure, notices)

module FrameVariableLengthExpandedRaw =

    let private parseApl ci source =
        decoder {
            let! parsed =
                AplParser.capture ci source

            return
                match parsed with
                | Ok apl -> AplExpansionRaw.Parsed apl
                | Error failures -> AplExpansionRaw.Invalid failures
        }

    let private fromMode5Outcome ci =
        function
        | Mode5ExpansionOutcome.Unprotected source ->
            parseApl ci source

        | Mode5ExpansionOutcome.Protected (layout, failure) ->
            decodePassed (
                AplExpansionRaw.Protected {
                    OriginalPayload = layout.OriginalPayload
                    EncryptedPart = layout.EncryptedPart
                    ClearSuffix = layout.ClearSuffix
                    Failure = failure
                }
            )

        | Mode5ExpansionOutcome.Invalid failures ->
            decodePassed (AplExpansionRaw.Invalid failures)

    let private expandComplete
        securityContext
        (raw: CompleteMessageRaw)
        : Decoder<CompleteMessageExpandedRaw> =

        let aplData =
            raw.Tpl.Value
            |> Tpl.rawAplData
            |> AplDataRaw.toByteField

        let ci =
            match raw.Tpl.Value with
            | TplRaw.NoneHeader tpl -> NoneTplHeader tpl.Ci.Value
            | TplRaw.ShortHeader tpl -> ShortTplHeader tpl.Ci.Value
            | TplRaw.LongHeader tpl -> LongTplHeader tpl.Ci.Value

        decoder {
            let! payload =
                match raw.Tpl.Value with
                | TplRaw.LongHeader {
                    Header = { Value = LongHeaderRaw.Mode5Raw header }
                  } ->
                    decoder {
                        let! outcome =
                            Mode5.expandLongHeader securityContext header aplData

                        return! fromMode5Outcome ci outcome
                    }

                | TplRaw.ShortHeader {
                    Header = { Value = ShortHeaderRaw.Mode5Raw _ }
                  } ->
                    PipelineIssues.single
                        raw.Tpl
                        "Short-header Mode 5 is standard-conformant, but this wired frame model does not contain the complete link-layer meter identification required to construct the IV. EN 13757-3:2025, G.5.4; EN 13757-7:2018, 9.4.4.1, Table 48."
                    |> AplExpansionRaw.Invalid
                    |> decodePassed

                | TplRaw.ShortHeader {
                    Header = { Value = ShortHeaderRaw.OtherModeRaw header }
                  } ->
                    PipelineIssues.single
                        header.Cnf
                        $"Security mode {header.Mode} is unsupported. EN 13757-7:2018, 7.5.8, Table 19."
                    |> AplExpansionRaw.Invalid
                    |> decodePassed

                | TplRaw.LongHeader {
                    Header = { Value = LongHeaderRaw.OtherModeRaw header }
                  } ->
                    PipelineIssues.single
                        header.Cnf
                        $"Security mode {header.Mode} is unsupported. EN 13757-7:2018, 7.5.8, Table 19."
                    |> AplExpansionRaw.Invalid
                    |> decodePassed

                | _ ->
                    parseApl ci aplData

            return {
                Dll = raw.Dll
                Tpl = raw.Tpl
                Payload = payload
            }
        }

    let expand
        securityContext
        (raw: Field<FrameVariableLengthRaw>)
        : Decoder<Field<FrameVariableLengthExpandedRaw>> =

        match raw.Value with
        | FrameVariableLengthRaw.CompleteMessage complete ->
            decoder {
                let! expanded =
                    expandComplete securityContext complete

                return
                    raw
                    |> Field.withValue (
                        FrameVariableLengthExpandedRaw.CompleteMessage expanded
                    )
            }

module ProtectedApl =

    let fromRaw (raw: ProtectedAplRaw) : Validation<ProtectedApl> =
        let original = raw.OriginalPayload.Span
        let encrypted = raw.EncryptedPart.Span

        let inOriginal span =
            span.Source = original.Source
            && span.Offset >= original.Offset
            && span.Offset + span.Length <= original.Offset + original.Length

        validator {
            let! () =
                ensure
                    raw.EncryptedPart
                    "Protected encrypted range is outside the original payload."
                    (inOriginal encrypted)

            and! () =
                match raw.ClearSuffix with
                | None -> passed ()
                | Some suffix ->
                    ensure
                        suffix
                        "Protected clear-suffix range is outside the original payload."
                        (inOriginal suffix.Span
                         && suffix.Span.Offset = encrypted.Offset + encrypted.Length)

            return {
                OriginalPayload = raw.OriginalPayload
                EncryptedPart = raw.EncryptedPart
                ClearSuffix = raw.ClearSuffix
                Failure = raw.Failure
            }
        }

module AplContent =

    let fromExpandedRaw =
        function
        | AplExpansionRaw.Parsed apl ->
            Apl.fromRaw apl
            |> map AplContent.Decoded

        | AplExpansionRaw.Protected raw ->
            ProtectedApl.fromRaw raw
            |> map AplContent.Protected

        | AplExpansionRaw.Invalid failures ->
            Failed (failures, [])

module FrameVariableLength =

    let private completeFromExpandedRaw
        (raw: CompleteMessageExpandedRaw)
        : Validation<CompleteMessage> =
        validator {
            let! dll = DllVariableLength.fromRaw raw.Dll
            and! tpl = Tpl.fromRaw raw.Tpl
            and! payload = AplContent.fromExpandedRaw raw.Payload

            return {
                Dll = dll
                Tpl = tpl
                Payload = payload
            }
        }

    let fromExpandedRaw
        (raw: Field<FrameVariableLengthExpandedRaw>)
        : Validation<Field<FrameVariableLength>> =

        match raw.Value with
        | FrameVariableLengthExpandedRaw.CompleteMessage complete ->
            completeFromExpandedRaw complete
            |> map (fun value ->
                raw
                |> Field.withValue (
                    FrameVariableLength.CompleteMessage value
                ))
