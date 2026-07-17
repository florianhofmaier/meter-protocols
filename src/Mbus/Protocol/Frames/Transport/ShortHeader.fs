namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type ShortHeaderMode0Raw =
    {
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigurationFieldBitsRaw>
    }

type ShortHeaderMode5Raw =
    {
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigurationFieldBitsRaw>
    }

type ShortHeaderRaw =
    | Mode0Raw of ShortHeaderMode0Raw
    | Mode5Raw of ShortHeaderMode5Raw

module ShortHeaderRaw =

    let parse : Parser<Field<ShortHeaderRaw>> =
        parseField "Short Tpl Header"
        <| parser {
            let! acc = AccessNumberRaw.parse
            let! status = StatusByteRaw.parse
            let! cnf = ConfigurationFieldRaw.parse

            match cnf.Value with
            | ConfigurationFieldRaw.Mode0Raw bits ->
                return
                    Mode0Raw
                        {
                            Acc = acc
                            Status = status
                            Cnf = bits
                        }

            | ConfigurationFieldRaw.Mode5Raw bits ->
                return
                    Mode5Raw
                        {
                            Acc = acc
                            Status = status
                            Cnf = bits
                        }
        }

type ShortHeaderMode0 =
    {
        Acc: Field<AccessNumber>
        Status: Field<StatusByte>
        Cnf: Field<ConfigurationFieldMode0>
    }

type ShortHeaderMode5 =
    {
        Acc: Field<AccessNumber>
        Status: Field<StatusByte>
        Cnf: Field<ConfigurationFieldMode5>
    }

type ShortHeader =
    | Mode0 of ShortHeaderMode0
    | Mode5 of ShortHeaderMode5

module ShortHeader =

    let fromRaw
        (raw: Field<ShortHeaderRaw>)
        : Validation<Field<ShortHeader>> =

        validator {
            match raw.Value with
            | Mode0Raw header ->
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode0.fromRaw header.Cnf
                return
                    raw
                    |> Field.withValue (
                        Mode0 {
                            Acc = acc
                            Status = status
                            Cnf = cnf
                        }
                    )

            | Mode5Raw header ->
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    raw
                    |> Field.withValue (
                        Mode5 {
                            Acc = acc
                            Status = status
                            Cnf = cnf
                        }
                    )
        }
