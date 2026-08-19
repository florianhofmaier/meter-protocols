namespace Metering.Mbus.Protocol.Frames.ExtendedLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.TransportLayer

type EllNoEncryptionRaw =
    {
        Ci: Field<CiField>
        Cc: Field<CcFieldRaw>
        Acc: Field<AccessNumberRaw>
    }

type EllRaw =
    | NoDllEncryption of EllNoEncryptionRaw

module EllNoEncryptionRaw =

    let parse ci: Parser<EllNoEncryptionRaw> =
        parser {
            let! cc = CcFieldRaw.parse
            let! acc = AccessNumberRaw.parse
            return
                {
                    Ci = ci
                    Cc = cc
                    Acc = acc
                }
        }

module EllRaw =

    let parse : Parser<Field<EllRaw>> =
        parseField "Extended Link Layer"
        <| parser {
            let! ci = CiField.parse

            match ci.Value with
            | Ell ell ->

                match ell with
                | CiFieldEll.NoDllEncryption ->

                    return!
                        EllNoEncryptionRaw.parse ci
                        |>> EllRaw.NoDllEncryption

            | _ ->
                let code = CiField.code ci.Value
                return!
                    failBefore
                    1
                    $"Expect CI-Field for ELL, got 0x{code:X2} instead."
        }

    let tryParse : Parser<Field<EllRaw> option> =
        parser {
            let! ci = CiField.peek

            match ci with
            | Ell _ ->
                let! ell = parse
                return Some ell

            | _ ->
                return None
        }

type EllNoEncryption =
    {
        Cc: Field<CcField>
        Acc: Field<AccessNumber>
    }

type Ell =
    | NoDllEncryption of EllNoEncryption

module Ell =

    let fromRaw (raw: Field<EllRaw>) : Validation<Ell> =

        validator {
            match raw.Value with
            | EllRaw.NoDllEncryption ell ->
                let! cc =
                    ell.Cc
                    |> validateField CcField.fromRaw

                and! acc =
                    ell.Acc
                    |> validateField AccessNumber.fromRaw

                return NoDllEncryption {
                    Cc = cc
                    Acc = acc
                }
        }