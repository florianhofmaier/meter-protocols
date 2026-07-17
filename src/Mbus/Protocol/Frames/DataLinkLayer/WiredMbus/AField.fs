namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type AFieldRaw =
    private PrimaryAddress of uint8

module AFieldRaw =

    let value (PrimaryAddress value) =
        value

    let parse : Parser<Field<AFieldRaw>> =
        parseField "PrimaryAddress" parseU8
        |>> Field.map PrimaryAddress

type PrimAdr =
    private PrimAdr of uint8

module PrimAdr =

    let min = 1uy
    let max = 250uy

    let value (PrimAdr value) =
        value

    let tryCreate b =
        if b < min || b > max then
            None
        else
            Some (PrimAdr b)

type AField =
    | Unconfigured
    | Configured of PrimAdr
    | RepeaterMgmt
    | SelectionOfDevice
    | Diagnosis
    | Broadcast

module AField =

    let fromRaw
        (raw: Field<AFieldRaw>)
        : Validation<Field<AField>> =

        let value = AFieldRaw.value raw.Value

        match AFieldRaw.value raw.Value with
            | 0uy ->
                raw |> Field.withValue Unconfigured |> passed

            | 251uy ->
                raw |> Field.withValue RepeaterMgmt |> passed

            | 253uy ->
                raw |> Field.withValue SelectionOfDevice |> passed

            | 254uy ->
                raw |> Field.withValue Diagnosis |> passed

            | 255uy ->
                raw |> Field.withValue Broadcast |> passed

            | _ ->
                match PrimAdr.tryCreate value with
                | Some adr ->
                    raw
                    |> Field.withValue (Configured adr)
                    |> passed

                | None ->
                    failed raw $"Invalid AField value: 0x{value:X2}"
