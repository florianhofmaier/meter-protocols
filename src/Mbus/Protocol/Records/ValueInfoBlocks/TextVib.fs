module Metering.Mbus.Protocol.Records.ValueInfoBlocks.TextVib

open System.Text
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.ValueInfoBlocks.VibCommon

let validate (raw: Field<InfoBlockRaw>) : Validation<string> =

    let bytes = vibBytes raw

    if bytes.Span[0] <> 0x7Cuy then
        failed raw "Extensions on plain-text VIF are not represented by the current Text VIB model"
    elif bytes.Length < 2 then
        failed raw "Text VIF (0x7C) is missing the text length byte"
    else
        let len = int bytes.Span[1]

        if bytes.Length <> 2 + len then
            failed
                raw
                $"Text VIF declares {len} byte(s), but VIB contains only {max 0 (bytes.Length - 2)} text byte(s)"
        elif bytes.Slice(2, len).ToArray() |> Array.exists (fun b -> b > 127uy) then
            failed raw "Text VIF requires ASCII"
        else
            bytes.Slice(2, len).ToArray() |> Array.rev |> Encoding.ASCII.GetString |> passed
