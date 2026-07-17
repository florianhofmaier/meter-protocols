namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.Application
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security

module FrameTpl =

    let private invalidShortMode5
        (cnf: ParsedField<ConfigurationFieldBitsRaw>)
        : Validation<_> =

        failed
            cnf
            "configuration field selects security mode 5 with short TPL header, but this wired M-Bus frame does not contain the link-layer meter address fields required for the mode 5 IV"

    let ci =
        function
        | FrameTpl.NoneHeader tpl ->
            NoneTplHeader tpl.Ci

        | FrameTpl.ShortHeaderMode0 tpl ->
            ShortTplHeader tpl.Ci

        | FrameTpl.LongHeaderMode0 tpl ->
            LongTplHeader tpl.Ci

        | FrameTpl.LongHeaderMode5 tpl ->
            LongTplHeader tpl.Ci

    let aplData =
        function
        | FrameTpl.NoneHeader tpl ->
            tpl.AplData

        | FrameTpl.ShortHeaderMode0 tpl ->
            tpl.AplData

        | FrameTpl.LongHeaderMode0 tpl ->
            tpl.AplData

        | FrameTpl.LongHeaderMode5 tpl ->
            tpl.AplData

    let fromRaw
        (raw: ParsedField<TplRaw>)
        : Validation<FrameTpl> =

        validator {
            match raw.Value with
            | TplRaw.NoneHeader tpl ->
                let! tpl =
                    TplWithNoneHeader.fromRaw tpl

                return FrameTpl.NoneHeader tpl

            | TplRaw.ShortHeader tpl ->
                match tpl.Header.Value with
                | ShortHeaderRaw.Mode0Raw _ ->
                    let! tpl =
                        TplWithShortHeader.fromRaw tpl

                    match tpl.Header with
                    | ShortHeader.Mode0 header ->
                        return
                            FrameTpl.ShortHeaderMode0
                                {
                                    Ci = tpl.Ci
                                    Header = header
                                    AplData = tpl.AplData
                                }

                    | ShortHeader.Mode5 _ ->
                        return!
                            failed raw "validated short TPL header mode does not match raw mode"

                | ShortHeaderRaw.Mode5Raw header ->
                    return!
                        invalidShortMode5 header.Cnf

            | TplRaw.LongHeader tpl ->
                let! tpl =
                    TplWithLongHeader.fromRaw tpl

                match tpl.Header with
                | LongHeader.Mode0 header ->
                    return
                        FrameTpl.LongHeaderMode0
                            {
                                Ci = tpl.Ci
                                Header = header
                                AplData = tpl.AplData
                            }

                | LongHeader.Mode5 header ->
                    return
                        FrameTpl.LongHeaderMode5
                            {
                                Ci = tpl.Ci
                                Header = header
                                AplData = tpl.AplData
                            }
        }


module VariableLength =

    let private aplBytes
        (aplData: ParsedField<AplDataRaw>)
        : ParsedField<ReadOnlyMemory<byte>> =

        AplDataRaw.toByteField aplData

    let private encryptionFailed
        (field: ParsedField<_>)
        (message: string)
        : Decoder<_> =

        {
            FieldId = field.Id
            Message = message
        }
        |> EncryptionFailed
        |> decodeError

    let private requireMode5Context
        (field: ParsedField<_>)
        (securityContext: SecurityContext)
        : Decoder<Mode5SecurityContext> =

        decoder {
            match securityContext with
            | SecurityContext.Mode5 mode5 ->
                return mode5

            | SecurityContext.NoSecurity ->
                return!
                    encryptionFailed
                        field
                        "security mode 5 requires a mode 5 security context"
        }

type FrameRaw =
    | SingleCharacter of ParsedField<unit>
    | FixedLength of ParsedField<FixedLengthFrameRaw>
    | VariableLength of ParsedField<VariableLengthFrameRaw>

module FrameRaw =

    let parse : Parser<ParsedField<FrameRaw>> =
        parseField "Frame"
        <| parser {
            let! startByte = peekU8

            match startByte with
            | 0x10uy ->
                return! FixedLengthFrameRaw.parse |>> FixedLength

            | 0x68uy ->
                return! VariableLengthFrameRaw.parse |>> VariableLength

            | 0xE5uy ->
                return! SingleCharacterFrame.parse |>> SingleCharacter

            | b ->
                return! fail $"Invalid start byte: 0x%02X{b}"
        }

type Frame =
    | SingleCharacter
    | FixedLength of FixedLengthFrame
    | VariableLength of VariableLengthFrame

module Frame =

    let decode
        (securityContext: SecurityContext)
        (source: ParsedField<ReadOnlyMemory<byte>>)
        : Decoder<Frame> =

        decoder {
            let! raw =
                parse FrameRaw.parse source

            match raw.Value with
            | FrameRaw.SingleCharacter _ ->
                return
                    Frame.SingleCharacter

            | FrameRaw.FixedLength fixedLength ->
                let! fixedLength =
                    validate FixedLengthFrame.fromRaw fixedLength

                return Frame.FixedLength fixedLength

            | FrameRaw.VariableLength variableLength ->
                let! variableLength =
                    VariableLengthFrame.decode securityContext variableLength

                return Frame.VariableLength variableLength
        }
