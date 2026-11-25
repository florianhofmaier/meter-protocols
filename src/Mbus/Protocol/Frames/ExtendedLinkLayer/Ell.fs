namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.TransportLayer

type EllNoEncryptionRaw = {
    Ci: Field<CiField>
    Cc: Field<CcFieldRaw>
    Acc: Field<AccessNumberRaw>
}

type EllRaw = NoDllEncryption of EllNoEncryptionRaw

module EllNoEncryptionRaw =

    let parse ci : Parser<EllNoEncryptionRaw> =
        parser {
            let! cc = CcFieldRaw.parse
            let! acc = AccessNumberRaw.parse
            return { Ci = ci; Cc = cc; Acc = acc }
        }

module EllRaw =

    let parse: Parser<Field<EllRaw>> =
        parseField "Extended Link Layer"
        <| parser {
            let! ci = CiField.parse

            match ci.Value with
            | Ell ell ->

                match ell with
                | CiFieldEll.NoDllEncryption ->

                    return! EllNoEncryptionRaw.parse ci |>> EllRaw.NoDllEncryption

            | _ ->
                let code = CiField.code ci.Value
                return! failBefore 1 $"Expect CI-Field for ELL, got 0x{code:X2} instead."
        }

    let tryParse: Parser<Field<EllRaw> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | Ell _ ->
                let! ell = parse
                return Some ell

            | _ -> return None
        }

type EllNoEncryption = {
    Ci: Field<CiField>
    Cc: Field<CcField>
    Acc: Field<AccessNumber>
}

type Ell = NoDllEncryption of EllNoEncryption

module Ell =

    let validate (raw: Field<EllRaw>) : Validation<Ell> =

        validator {
            match raw.Value with
            | EllRaw.NoDllEncryption ell ->
                let! cc = ell.Cc |> validateField CcField.validate

                and! acc = ell.Acc |> validateField AccessNumber.validate

                return NoDllEncryption { Ci = ell.Ci; Cc = cc; Acc = acc }
        }

    /// Writes the supported ELL header without consuming or generating following layers.
    let write (ell: Ell) : Writer<unit> =
        match ell with
        | NoDllEncryption header ->
            writer {
                if header.Ci.Value <> CiField.Ell CiFieldEll.NoDllEncryption then
                    return! writerError "ELL CI does not match the modeled header"

                do! CiField.write header.Ci.Value
                do! CcField.write header.Cc.Value
                do! AccessNumber.write header.Acc.Value
            }
