namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type ShortHeaderMode0Raw =
    {
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
    }

type ShortHeaderMode5Raw =
    {
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
        Verification: Field<DecryptionVerificationRaw>
    }

type ShortHeaderOtherModeRaw =
    {
        Mode: byte
        Acc: Field<AccessNumberRaw>
        Status: Field<StatusByteRaw>
        Cnf: Field<ConfigFieldBitsRaw>
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
            let! cnf = ConfigFieldRaw.parse

            match cnf.Value with
            | ConfigFieldRaw.Mode0Raw bits ->
                return
                    Mode0Raw
                        {
                            Acc = acc
                            Status = status
                            Cnf = bits
                        }

            | ConfigFieldRaw.Mode5Raw bits ->
                let! verification = DecryptionVerificationRaw.parse
                return
                    Mode5Raw
                        {
                            Acc = acc
                            Status = status
                            Cnf = bits
                            Verification = verification
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
                    Mode0 {
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }
                    |> Field.withValue raw

            | Mode5Raw header ->
                let! acc = AccessNumber.fromRaw header.Acc
                and! status = StatusByte.fromRaw header.Status
                and! cnf = ConfigurationFieldMode5.fromRaw header.Cnf
                return
                    Mode5 {
                        Acc = acc
                        Status = status
                        Cnf = cnf
                    }
                    |> Field.withValue raw
        }
