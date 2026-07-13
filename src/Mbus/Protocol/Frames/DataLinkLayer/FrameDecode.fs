namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security

type DecodedLinkUserData =
    {
        Tpl : Tpl
        AplData : ParsedField<ReadOnlyMemory<byte>>
    }

type DecodedVariableLength =
    {
        CField : CField
        AField : AField
        LinkUserData : DecodedLinkUserData
    }

type DecodedFrame =
    | Confirmation
    | FixedLength of FrameFixedLength
    | VariableLength of DecodedVariableLength

module FrameDecode =

    let private decodeVariableLength
        (securityContext: SecurityContext)
        (raw: ParsedField<VariableLengthRaw>)
        : Decoder<DecodedVariableLength> =

        decoder {
            let! variableLength =
                validate VariableLength.fromRaw raw

            let tpl =
                variableLength.LinkUserData.Tpl

            let! aplData =
                AplProtection.unprotect securityContext tpl

            return {
                CField = variableLength.CField
                AField = variableLength.AField
                LinkUserData =
                    {
                        Tpl = tpl
                        AplData = aplData
                    }
            }
        }

    let fromRaw
        (securityContext: SecurityContext)
        (raw: ParsedField<FrameRaw>)
        : Decoder<DecodedFrame> =

        decoder {
            match raw.Value with
            | FrameRaw.Confirmation _ ->
                return DecodedFrame.Confirmation

            | FrameRaw.FixedLength fixedLength ->
                let! fixedLength =
                    validate FrameFixedLength.fromRaw fixedLength

                return DecodedFrame.FixedLength fixedLength

            | FrameRaw.VariableLength variableLength ->
                let! variableLength =
                    decodeVariableLength securityContext variableLength

                return DecodedFrame.VariableLength variableLength
        }

    let decode
        (securityContext: SecurityContext)
        (source: ParsedField<ReadOnlyMemory<byte>>)
        : Decoder<DecodedFrame> =

        decoder {
            let! raw =
                parse FrameRaw.parse source

            return!
                fromRaw securityContext raw
        }
