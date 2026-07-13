namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type CrcBytes =
    private CrcBytes of ReadOnlyMemory<uint8>

module CrcBytes =

    let bytes (CrcBytes bytes) =
        bytes

    let create bytes =
        CrcBytes bytes

module CrcCalculator =

    let calculate (CrcBytes bytes) =
        let mutable sum = 0x00uy

        for b in bytes.Span do
            sum <- sum + b

        sum

type Crc =
    private Crc of uint8

module Crc =

    let value (Crc v) =
        v

    let parse : Parser<ParsedField<Crc>> =
        parseField "CRC" parseU8
        |>> ParsedField.map Crc

    let validate (raw: ParsedField<Crc>) (crcBytes: CrcBytes) =
        let expectedCrc = CrcCalculator.calculate crcBytes
        let actualCrc = value raw.Value

        if expectedCrc <> actualCrc then
            failed raw $"CRC mismatch. Expected 0x{expectedCrc:X2}, but got 0x{actualCrc:X2}"
        else
            passed ()
