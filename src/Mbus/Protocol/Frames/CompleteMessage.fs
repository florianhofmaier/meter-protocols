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
    | NotExpandedForUnsupportedSecurityMode of mode: byte * Field<ConfigurationFieldBitsRaw>
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

    type private WiredCompleteMessageCi =
        | Supported
        | ShortTplHeader
        | Afl
        | ReservedOrUnsupported

    let private classifyCi =
        function
        | 0x50uy
        | 0x51uy
        | 0x52uy
        | 0x53uy
        | 0x72uy
        | 0x75uy ->
            Supported

        // EN 13757-7:2018, 5.2, Table 2 marks these as short-header CI values.
        | 0x5Auy
        | 0x61uy
        | 0x65uy
        | 0x67uy
        | 0x6Auy
        | 0x6Euy
        | 0x74uy
        | 0x7Auy
        | 0x7Buy
        | 0x7Duy
        | 0x7Fuy
        | 0x88uy
        | 0x8Auy
        | 0x9Euy
        | 0xC1uy
        | 0xC4uy ->
            ShortTplHeader

        | 0x90uy ->
            Afl

        | _ ->
            ReservedOrUnsupported

    let private rejectCi
        (source: Field<ReadOnlyMemory<byte>>)
        ci
        message =

        decodeError (
            ParseFailed {
                Source = source.Span.Source
                Pos = source.Span.Offset
                Msg = $"{message} Actual TPL.CI=0x{ci:X2}."
            }
        )

    let parseCompleteMessageFromDll
        (dll: Field<DllVariableLengthRaw>)
        : Decoder<Field<FrameVariableLengthRaw>> =

        let source =
            dll.Value.UserData.Value.HigherLayerData

        if source.Value.IsEmpty then
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
        else
            let ci = source.Value.Span[0]

            match classifyCi ci with
            | Supported ->
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

            | ShortTplHeader ->
                rejectCi
                    source
                    ci
                    ("Short TPL headers are not permitted in the wired M-Bus complete-message path. "
                     + "EN 13757-3:2025, Clause 5; EN 13757-7:2018, 5.2, Table 2.")

            | Afl ->
                rejectCi
                    source
                    ci
                    ("Unsupported but standard-conformant AFL variant at HigherLayerData; "
                     + "the supported path is a complete TPL message. EN 13757-7:2018, 5.2, Table 2.")

            | ReservedOrUnsupported ->
                let classification =
                    if ci = 0x57uy then
                        "Reserved CI value in the current EN 13757 path"
                    else
                        "Reserved or unsupported CI value in the wired complete-message path"

                rejectCi
                    source
                    ci
                    $"{classification}. EN 13757-7:2018, 5.2, Table 2."

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
                    Header = { Value = ShortHeaderRaw.OtherModeRaw header }
                  } ->
                    NotExpandedForUnsupportedSecurityMode (header.Mode, header.Cnf)
                    |> decodePassed

                | TplRaw.LongHeader {
                    Header = { Value = LongHeaderRaw.OtherModeRaw header }
                  } ->
                    NotExpandedForUnsupportedSecurityMode (header.Mode, header.Cnf)
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
        let originalEnd = original.Offset + original.Length
        let encryptedEnd = encrypted.Offset + encrypted.Length

        let inOriginal span =
            span.Source = original.Source
            && span.Offset >= original.Offset
            && span.Offset + span.Length <= originalEnd

        validator {
            let! () =
                ensure
                    raw.EncryptedPart
                    "Protected encrypted range is outside the original payload."
                    (inOriginal encrypted)

            and! () =
                ensure
                    raw.EncryptedPart
                    "Protected encrypted range must start at the original payload start."
                    (encrypted.Source = original.Source
                     && encrypted.Offset = original.Offset)

            and! () =
                match raw.ClearSuffix with
                | None ->
                    ensure
                        raw.EncryptedPart
                        "Protected encrypted range without a clear suffix must end at the original payload end."
                        (encrypted.Source = original.Source
                         && encryptedEnd = originalEnd)

                | Some suffix ->
                    validator {
                        let! () =
                            ensure
                                suffix
                                "Protected clear-suffix range is outside the original payload."
                                (inOriginal suffix.Span)

                        and! () =
                            ensure
                                suffix
                                "Protected clear suffix must start exactly after the encrypted range."
                                (suffix.Span.Source = original.Source
                                 && suffix.Span.Offset = encryptedEnd)

                        and! () =
                            ensure
                                suffix
                                "Protected clear suffix must end at the original payload end."
                                (suffix.Span.Source = original.Source
                                 && suffix.Span.Offset + suffix.Span.Length = originalEnd)

                        return ()
                    }

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

        | AplExpansionRaw.NotExpandedForUnsupportedSecurityMode (mode, field) ->
            failed
                field
                $"APL expansion was skipped for unsupported outer security mode {mode}."

        | AplExpansionRaw.Invalid failures ->
            Failed (failures, [])

module private CompleteMessageCrossLayer =

    type private Direction =
        | CommandToDevice
        | ResponseFromDevice

    let private ciAndDirection (raw: Field<TplRaw>) =
        match raw.Value with
        | TplRaw.NoneHeader tpl ->
            let ci = NoneTplHeader tpl.Ci.Value
            tpl.Ci |> Field.map (fun _ -> CiFieldTpl.value ci), CommandToDevice

        | TplRaw.ShortHeader tpl ->
            let ci = ShortTplHeader tpl.Ci.Value
            tpl.Ci |> Field.map (fun _ -> CiFieldTpl.value ci), ResponseFromDevice

        | TplRaw.LongHeader tpl ->
            let ci = LongTplHeader tpl.Ci.Value
            let direction =
                match tpl.Ci.Value with
                | ApplicationResetOrSelectLongHeader -> CommandToDevice
                | ResponseLongHeader
                | AlarmLongHeader -> ResponseFromDevice

            tpl.Ci |> Field.map (fun _ -> CiFieldTpl.value ci), direction

    let validateRaw (raw: CompleteMessageExpandedRaw) : Validation<unit> =
        let cField =
            raw.Dll.Value.UserData.Value.CField

        let cValue =
            CFieldRaw.value cField.Value

        let cDirection, cRole =
            if cValue &&& 0x40uy = 0x40uy then
                CommandToDevice,
                $"primary command (PRM=1, function=0x{cValue &&& 0x0Fuy:X1})"
            else
                ResponseFromDevice,
                $"secondary response (PRM=0, function=0x{cValue &&& 0x0Fuy:X1})"

        let ciField, ciDirection =
            ciAndDirection raw.Tpl

        let expected =
            match cDirection with
            | CommandToDevice -> "a command-to-device CI"
            | ResponseFromDevice -> "a response-from-device CI"

        ensure
            ciField
            ($"Cross-layer direction mismatch between DLL.UserData.CField and TPL.CI: "
             + $"C-field=0x{cValue:X2} is {cRole}, but CI=0x{ciField.Value:X2} has the opposite direction; "
             + $"the C-field role requires {expected}. EN 13757-7:2018, 5.2, Table 2.")
            (cDirection = ciDirection)

module FrameVariableLength =

    let private validatePayloadForRoot =
        function
        | AplExpansionRaw.NotExpandedForUnsupportedSecurityMode _ ->
            passed None

        | payload ->
            AplContent.fromExpandedRaw payload
            |> map Some

    let private completeFromExpandedRaw
        (raw: CompleteMessageExpandedRaw)
        : Validation<CompleteMessage> =
        validator {
            let! dll = DllVariableLength.fromRaw raw.Dll
            and! tpl = Tpl.fromRaw raw.Tpl
            and! payload = validatePayloadForRoot raw.Payload
            and! () = CompleteMessageCrossLayer.validateRaw raw

            match payload with
            | Some payload ->
                return {
                    Dll = dll
                    Tpl = tpl
                    Payload = payload
                }

            | None ->
                return!
                    failed
                        raw.Tpl
                        "TPL validation accepted an unsupported security mode whose APL expansion was skipped."
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
