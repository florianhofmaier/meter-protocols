namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.Application
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Security

type VariableLengthFrameRaw =
    {
        UserData: ParsedField<VariableLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: ParsedField<Crc>
        End: ParsedField<EndFieldRaw>
    }

module VariableLengthFrameRaw =

    let parse : Parser<ParsedField<VariableLengthFrameRaw>> =
        parseField "Variable Length"
        <| parser {
            let! _ = StartVariableLength.parse

            let! lField = LField.parse

            let! _ = StartVariableLength.parse

            let len =
                LField.value lField.Value
                |> int

            let! userDataStart = position

            let! userData = runOnSubSlice len VariableLengthUserDataRaw.parse

            let! crcBytes =
                bufferSliceAt userDataStart len
                |>> CrcBytes.create

            let! crc = Crc.parse

            let! endField = EndFieldRaw.parse

            return {
                UserData = userData
                CrcBytes = crcBytes
                Crc = crc
                End = endField
            }
        }

type private VariableLengthEnvelope =
    {
        CField: CField
        AField: AField
        Tpl: Tpl
    }

module private VariableLengthEnvelope =

    let private AvoidShortHeadersWithEncryption tpl =
        match tpl.Value with
        | TplRaw.ShortHeader
            { Header = { Value = ShortHeaderRaw.Mode5Raw _ } } ->
            failed
                tpl
                "Short headers with encryption are not allowed in variable length frames."
        | _ ->
            passed ()

    let fromRaw
        (raw: ParsedField<VariableLengthFrameRaw>)
        : Validation<VariableLengthEnvelope> =

        validator {
            let userData =
                raw.Value.UserData.Value

            let! cField =
                CField.fromRaw userData.CField

            and! aField =
                AField.fromRaw userData.AField

            and! tpl =
                Tpl.fromRaw userData.LinkUserData.Value.Tpl

            and! _ =
                AvoidShortHeadersWithEncryption userData.LinkUserData.Value.Tpl

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return {
                CField = cField
                AField = aField
                Tpl = tpl
            }
        }

type VariableLengthFrame =
    {
        CField: CField
        AField: AField
        Tpl: Tpl
        Apl: Apl
    }

module VariableLengthFrame =

    let private unprotectAplData
        (securityContext: SecurityContext)
        (tpl: Tpl)
        : Decoder<ParsedField<ReadOnlyMemory<byte>>> =

        decoder {
            match tpl with
            | Tpl.LongHeader ({ Header = LongHeader.Mode5 header } as longTpl) ->
                return!
                    Mode5.unprotect
                        securityContext
                        header.Device
                        header.Acc
                        header.Cnf
                        (AplDataRaw.toByteField longTpl.AplData)

            | _ ->
                return
                    tpl
                    |> Tpl.aplData
                    |> AplDataRaw.toByteField
        }

    let decode
        (securityContext: SecurityContext)
        (raw: ParsedField<VariableLengthFrameRaw>)
        : Decoder<VariableLengthFrame> =

        decoder {
            let! envelope =
                validate VariableLengthEnvelope.fromRaw raw

            let! aplData =
                unprotectAplData securityContext envelope.Tpl

            let! aplRaw =
                parse (AplRaw.parse (Tpl.ci envelope.Tpl)) aplData

            let! apl =
                validate Apl.fromRaw aplRaw

            return {
                CField = envelope.CField
                AField = envelope.AField
                Tpl = envelope.Tpl
                Apl = apl
            }
        }