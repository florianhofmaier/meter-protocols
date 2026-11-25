namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type CcFieldRaw = private CcField of uint8

module CcFieldRaw =

    let value (CcField v) = v

    let parse: Parser<Field<CcFieldRaw>> =
        parseField "CC-Field" parseU8 |>> Field.map CcField

type CcField = {
    XField: bool
    RField: bool
    AField: bool
    PField: bool
    HField: bool
    SField: bool
    DField: bool
    BField: bool
}

module CcField =

    let validate (raw: Field<CcFieldRaw>) : Validation<CcField> =
        let bits = CcFieldRaw.value raw.Value

        passed {
            XField = (bits &&& 0x01uy) <> 0uy
            RField = (bits &&& 0x02uy) <> 0uy
            AField = (bits &&& 0x04uy) <> 0uy
            PField = (bits &&& 0x08uy) <> 0uy
            HField = (bits &&& 0x10uy) <> 0uy
            SField = (bits &&& 0x20uy) <> 0uy
            DField = (bits &&& 0x40uy) <> 0uy
            BField = (bits &&& 0x80uy) <> 0uy
        }

    /// Writes the eight communication-control flags in standard bit positions.
    let write (cc: CcField) : Writer<unit> =
        let bits =
            (if cc.XField then 1uy else 0uy)
            ||| (if cc.RField then 2uy else 0uy)
            ||| (if cc.AField then 4uy else 0uy)
            ||| (if cc.PField then 8uy else 0uy)
            ||| (if cc.HField then 16uy else 0uy)
            ||| (if cc.SField then 32uy else 0uy)
            ||| (if cc.DField then 64uy else 0uy)
            ||| (if cc.BField then 128uy else 0uy)

        writeU8 bits
