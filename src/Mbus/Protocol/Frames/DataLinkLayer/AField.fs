namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

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

    let parse : Parser<ParsedField<AFieldRaw>> =
        parseField "PrimaryAddress" parseU8
        |>> ParsedField.map PrimaryAddress

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
        (raw: ParsedField<AFieldRaw>)
        : Validation<AField> =

        let value = AFieldRaw.value raw.Value

        match AFieldRaw.value raw.Value with
            | 0uy ->
                passed Unconfigured

            | 251uy ->
                passed RepeaterMgmt

            | 253uy ->
                passed SelectionOfDevice

            | 254uy ->
                passed Diagnosis

            | 255uy ->
                passed Broadcast

            | _ ->
                match PrimAdr.tryCreate value with
                | Some adr ->
                    passed (Configured adr)

                | None ->
                    failed raw $"Invalid AField value: 0x{value:X2}"
