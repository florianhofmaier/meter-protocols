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

type ShortHeaderOtherModeRaw =
    {
        Mode: byte
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigurationFieldBitsRaw>
    }

type ShortHeaderRaw =
    | Mode0Raw of ShortHeaderMode0Raw
    | Mode5Raw of ShortHeaderMode5Raw
    | OtherModeRaw of ShortHeaderOtherModeRaw

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

            | ConfigurationFieldRaw.OtherModeRaw (mode, bits) ->
                return
                    OtherModeRaw
                        {
                            Mode = mode
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
                and! status = StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode0.fromRaw header.Cnf
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
                and! status = StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    raw
                    |> Field.withValue (
                        Mode5 {
                            Acc = acc
                            Status = status
                            Cnf = cnf
                        }
                    )

            | OtherModeRaw header ->
                let! _acc = AccessNumber.fromRaw header.Acc
                and! _status = StatusByte.fromRaw header.Status
                and! unsupported : Field<ShortHeader> =
                    failed
                        header.Cnf
                        $"Security mode {header.Mode} is standard-defined or reserved but unsupported by this decoder. EN 13757-7:2018, 7.5.8, Table 19."

                return unsupported
        }
