namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.TransportLayer

[<RequireQualifiedAccess>]
type MbusProtectionInput =
    | Plain of Field<ReadOnlyMemory<byte>>
    | Mode5 of Mode5ProtectionInput

module MbusProtectionInput =

    /// Analyzer boundary: validates the concrete protection prerequisites.
    let decodeRaw (raw: Field<TplRaw>) : Decoder<MbusProtectionInput> =
        let payload = TplRaw.aplData raw.Value

        let plain (bits: Field<ConfigurationFieldBitsRaw>) expected =
            decoder {
                let! () =
                    Decoder.validate
                        (fun field ->
                            ensure
                                field
                                "Protection mode disagrees with the parsed header variant."
                                (Mode.tryMap (ConfigurationFieldBitsRaw.value field.Value) =
                                    Some expected))
                        bits

                return MbusProtectionInput.Plain payload
            }

        let hasEncryptedBytes bits =
            ConfigurationFieldBitsRaw.value bits.Value
            |> NumberOfEncryptedBlocks.map
            |> NumberOfEncryptedBlocks.value
            |> (<>) 0

        match raw.Value with
        | TplRaw.LongHeader {
                                Header = {
                                             Value = LongHeaderRaw.Mode5Raw header
                                         }
                            } when not (hasEncryptedBytes header.Cnf) -> plain header.Cnf Mode.Mode5
        | TplRaw.LongHeader {
                                Header = {
                                             Value = LongHeaderRaw.Mode5Raw header
                                         }
                            } ->
            decoder {
                let! input = Mode5.decodeInputRaw header payload
                return MbusProtectionInput.Mode5 input
            }
        | TplRaw.ShortHeader {
                                 Header = {
                                              Value = ShortHeaderRaw.Mode5Raw header
                                          }
                             } when not (hasEncryptedBytes header.Cnf) ->
            plain header.Cnf Mode.Mode5
        | TplRaw.ShortHeader {
                                 Header = { Value = ShortHeaderRaw.Mode5Raw _ }
                             } ->
            Decoder.decodeError (
                EncryptionFailed {
                    FieldId = raw.Id
                    Message = "Mode 5 protection requires meter identity from a long TPL header."
                }
            )
        | TplRaw.LongHeader {
                                Header = {
                                             Value = LongHeaderRaw.Mode0Raw header
                                         }
                            } -> plain header.Cnf Mode.Mode0
        | TplRaw.ShortHeader {
                                 Header = {
                                              Value = ShortHeaderRaw.Mode0Raw header
                                          }
                             } -> plain header.Cnf Mode.Mode0
        | TplRaw.NoneHeader _ -> Decoder.decodePassed (MbusProtectionInput.Plain payload)

    /// Selects and prepares the operation from validated TPL fields, without keys.
    let decode (tpl: Tpl) : Decoder<MbusProtectionInput> =
        let payload = Tpl.aplData tpl

        match tpl with
        | Tpl.LongHeader {
                             Header = { Value = LongHeader.Mode5 header }
                         } ->
            decoder {
                let! input = Mode5.decodeInput header payload
                return MbusProtectionInput.Mode5 input
            }
        | _ -> Decoder.decodePassed (MbusProtectionInput.Plain payload)

module MbusProtection =

    let unprotect resolver input : Result<ReadOnlyMemory<byte>, EncryptionError> =
        match input with
        | MbusProtectionInput.Plain bytes -> Ok bytes.Value
        | MbusProtectionInput.Mode5 input -> Mode5.unprotect resolver input

    let decode resolver input : Decoder<Field<ReadOnlyMemory<byte>>> =
        match input with
        | MbusProtectionInput.Plain bytes -> Decoder.decodePassed bytes
        | MbusProtectionInput.Mode5 input -> Mode5.decode resolver input
