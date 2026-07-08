namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type ShortHeaderMode0Raw =
    {
        Acc: ParsedField<AccessNumberRaw>
        Status: ParsedField<StatusByteRaw>
        Cnf: ParsedField<ConfigurationFieldBitsRaw>
    }

type ShortHeaderMode5Raw =
    {
        Acc: ParsedField<AccessNumberRaw>
        Status: ParsedField<StatusByteRaw>
        Cnf: ParsedField<ConfigurationFieldBitsRaw>
    }

type ShortHeaderRaw =
    | Mode0Raw of ShortHeaderMode0Raw
    | Mode5Raw of ShortHeaderMode5Raw

module ShortHeaderRaw =

    let parse : Parser<ParsedField<ShortHeaderRaw>> =
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
        Acc: AccessNumber
        Status: StatusByte
        Cnf: ConfigurationFieldMode0
    }

type ShortHeaderMode5 =
    {
        Acc: AccessNumber
        Status: StatusByte
        Cnf: ConfigurationFieldMode5
    }

type ShortHeader =
    | Mode0 of ShortHeaderMode0
    | Mode5 of ShortHeaderMode5

module ShortHeader =

    let fromRaw
        (raw: ParsedField<ShortHeaderRaw>)
        : Validation<ShortHeader> =

        validator {
            match raw.Value with
            | Mode0Raw header ->
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode0.fromRaw header.Cnf
                return
                    Mode0 {
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }

            | Mode5Raw header ->
                let! acc = AccessNumber.fromRaw header.Acc
                let! status = StatusByte.fromRaw header.Status
                let! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    Mode5 {
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }
        }
