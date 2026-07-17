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
        UserData: Field<VariableLengthUserDataRaw>
        CrcBytes: CrcBytes
        Crc: Field<Crc>
        End: Field<EndFieldRaw>
    }

module VariableLengthFrameRaw =

    let parse : Parser<Field<VariableLengthFrameRaw>> =
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
        CField: Field<CField>
        AField: Field<AField>
        Tpl: Field<Tpl>
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
        (raw: Field<VariableLengthFrameRaw>)
        : Validation<Field<VariableLengthEnvelope>> =

        validator {
            let! userData =
                VariableLengthUserData.fromRaw raw.Value.UserData

            and! _ =
                AvoidShortHeadersWithEncryption raw.Value.UserData.Value.LinkUserData.Value.Tpl

            and! () = Crc.validate raw.Value.Crc raw.Value.CrcBytes

            and! () = EndField.validate raw.Value.End

            return
                raw
                |> Field.withValue {
                    CField = userData.Value.CField
                    AField = userData.Value.AField
                    Tpl = userData.Value.LinkUserData.Value.Tpl
                }
        }

type VariableLengthFrame =
    {
        CField: Field<CField>
        AField: Field<AField>
        Tpl: Field<Tpl>
        Apl: Field<Apl>
    }

module VariableLengthFrame =

    let private unprotectAplData
        (securityContext: SecurityContext)
        (tpl: Field<Tpl>)
        : Decoder<Field<ReadOnlyMemory<byte>>> =

        decoder {
            match tpl.Value with
            | Tpl.LongHeader ({ Header = { Value = LongHeader.Mode5 header } } as longTpl) ->
                return!
                    Mode5.unprotect
                        securityContext
                        header.Device
                        header.Acc
                        header.Cnf
                        (AplDataRaw.toByteField longTpl.AplData)

            | _ ->
                return
                    tpl.Value
                    |> Tpl.aplData
                    |> AplDataRaw.toByteField
        }

    let decode
        (securityContext: SecurityContext)
        (raw: Field<VariableLengthFrameRaw>)
        : Decoder<Field<VariableLengthFrame>> =

        decoder {
            let! envelope =
                validate VariableLengthEnvelope.fromRaw raw

            let! aplData =
                unprotectAplData securityContext envelope.Value.Tpl

            let! aplRaw =
                parse (AplRaw.parse (Tpl.ci envelope.Value.Tpl.Value)) aplData

            let! apl =
                validate Apl.fromRaw aplRaw

            return
                raw
                |> Field.withValue {
                    CField = envelope.Value.CField
                    AField = envelope.Value.AField
                    Tpl = envelope.Value.Tpl
                    Apl = apl
                }
        }
