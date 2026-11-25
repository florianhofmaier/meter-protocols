namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer

/// Frame-derived operation input, separate from external keys.
type Mode5ProtectionContext = {
    Device: DeviceIdentification
    AccessNumber: Field<AccessNumber>
}

/// Prepared byte ranges and frame inputs; constructed only after layout validation.
type Mode5ProtectionInput = private {
    Frame: Mode5ProtectionContext
    Payload: Field<ReadOnlyMemory<byte>>
    Cipher: Field<ReadOnlyMemory<byte>>
    Suffix: Field<ReadOnlyMemory<byte>>
} with

    member input.Context = input.Frame
    member input.Origin = input.Payload
    member input.Encrypted = input.Cipher
    member input.ClearSuffix = input.Suffix

module Mode5 =

    let private encryptedLength blocks payloadLength =
        // EN 13757-7, 7.6: all N bits set means the entire payload is encrypted.
        if blocks = 15 then
            payloadLength
        else
            blocks * AesCbc.blockLength

    /// Prepares the protected ranges without key lookup, decryption or a derived source.
    let private prepareInput
        (frame: Mode5ProtectionContext)
        (blocks: int)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Decoder<Mode5ProtectionInput> =

        let count = encryptedLength blocks payload.Value.Length

        if
            count > payload.Value.Length
            || count % AesCbc.blockLength <> 0
            || (blocks <> 0 && count < 16)
        then
            Decoder.decodeError (
                EncryptionFailed {
                    FieldId = payload.Id
                    Message =
                        "Mode 5 encrypted payload must contain complete 16-byte blocks within the payload."
                }
            )
        else
            decoder {
                let! encrypted, suffix =
                    Decoder.parseValue
                        (parser {
                            let! encrypted = parseField "Mode 5 encrypted data" (take count)
                            let! suffix = parseField "Mode 5 clear suffix" takeAll
                            return encrypted, suffix
                        })
                        payload

                return {
                    Frame = frame
                    Payload = payload
                    Cipher = encrypted
                    Suffix = suffix
                }
            }

    let decodeInput (header: LongHeaderMode5) payload =
        prepareInput
            {
                Device = header.Device
                AccessNumber = header.Acc
            }
            (NumberOfEncryptedBlocks.value header.Cnf.Value.NumOfEncryptedBlocks)
            payload

    /// Checks only inputs used by the operation (EN 13757-7, Table 48).
    /// Other header diagnostics remain the responsibility of TPL validation.
    let decodeInputRaw (header: LongHeaderMode5Raw) payload =
        let prerequisites =
            validator {
                let! device =
                    DeviceIdentification.validateElements
                        header.IdNum
                        header.Mfr
                        header.Version
                        header.DevType

                and! access = validateField AccessNumber.validate header.Acc

                and! () =
                    ensure
                        header.Cnf
                        "Protection requires security mode 5."
                        (Mode.tryMap (ConfigurationFieldBitsRaw.value header.Cnf.Value) =
                            Some Mode.Mode5)

                return {
                    Device = device
                    AccessNumber = access
                }
            }

        decoder {
            let! frame = Decoder.validate (fun _ -> prerequisites) header.Cnf

            let blocks =
                ConfigurationFieldBitsRaw.value header.Cnf.Value
                |> NumberOfEncryptedBlocks.map
                |> NumberOfEncryptedBlocks.value

            return! prepareInput frame blocks payload
        }

    let unprotect
        (resolver: IExternalSecurityContextResolver)
        (input: Mode5ProtectionInput)
        : Result<ReadOnlyMemory<byte>, EncryptionError> =

        let count = input.Encrypted.Value.Length

        if count = 0 then
            Ok input.Origin.Value
        else
            result {
                let! context =
                    Mode5OperationParameters.create
                        (DeviceIdentification.toMeterAddress input.Context.Device)
                        input.Context.AccessNumber.Value
                        resolver

                let! plain = AesCbc.decrypt context.Key context.Iv input.Encrypted.Value 0 count

                let! () =
                    if plain.Span[0] <> 0x2Fuy || plain.Span[1] <> 0x2Fuy then
                        Error(EncryptionError.create "Mode 5 decryption verification failed.")
                    else
                        Ok()

                let combined = Array.zeroCreate input.Origin.Value.Length
                plain.CopyTo(combined.AsMemory())
                input.ClearSuffix.Value.CopyTo(combined.AsMemory(count))
                return ReadOnlyMemory<byte> combined
            }

    let decode
        (resolver: IExternalSecurityContextResolver)
        (input: Mode5ProtectionInput)
        : Decoder<Field<ReadOnlyMemory<byte>>> =

        if input.Encrypted.Value.Length = 0 then
            Decoder.decodePassed input.Origin
        else
            match unprotect resolver input with
            | Error error ->
                Decoder.decodeError (
                    EncryptionFailed {
                        FieldId = input.Origin.Id
                        Message = EncryptionError.value error
                    }
                )
            | Ok plain ->
                Decoder.createDerivedSource
                    "Decrypted APL Data"
                    (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                    true
                    input.Origin
                    plain

    /// Protects M-Bus record bytes using the supplied meter identity and access number.
    /// EN 13757-7:2018, 9.4.4 and Table 48; EN 13757-3:2025, Table 6 (idle filler).
    /// Alignment filler follows the AES-check bytes and precedes records, keeping any MDH payload final.
    let protect resolver meter access (records: ReadOnlyMemory<byte>) =
        match Mode5OperationParameters.create meter access resolver with
        | Error error -> Error error
        | Ok context ->
            let length = ((records.Length + 2 + 15) / 16) * 16
            let plain = Array.create length 0x2Fuy
            records.CopyTo(plain.AsMemory(length - records.Length))

            try
                AesCbc.encrypt context.Key context.Iv (ReadOnlyMemory plain)
            finally
                System.Security.Cryptography.CryptographicOperations.ZeroMemory(plain.AsSpan())
