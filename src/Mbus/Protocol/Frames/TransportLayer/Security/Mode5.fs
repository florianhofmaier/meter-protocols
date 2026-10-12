namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer

/// Frame-derived operation input, separate from external keys.
type Mode5ProtectionContext =
    {
        Device: DeviceIdentification
        AccessNumber: Field<AccessNumber>
    }

/// Prepared byte ranges and frame inputs; constructed only after layout validation.
type Mode5ProtectionInput =
    private {
        Frame: Mode5ProtectionContext
        Payload: Field<ReadOnlyMemory<byte>>
        Cipher: Field<ReadOnlyMemory<byte>>
        Suffix: Field<ReadOnlyMemory<byte>>
    }
    member input.Context = input.Frame
    member input.Origin = input.Payload
    member input.Encrypted = input.Cipher
    member input.ClearSuffix = input.Suffix

module Mode5 =

    let private encryptedLength (header: LongHeaderMode5) payloadLength =
        let blocks = NumberOfEncryptedBlocks.value header.Cnf.Value.NumOfEncryptedBlocks
        // EN 13757-7, 7.6: all N bits set means the entire payload is encrypted.
        if blocks = 15 then payloadLength else blocks * AesCbc.blockLength

    /// Prepares the protected ranges without key lookup, decryption or a derived source.
    let decodeInput
        (header: LongHeaderMode5)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Decoder<Mode5ProtectionInput> =

        let count = encryptedLength header payload.Value.Length
        let blocks = NumberOfEncryptedBlocks.value header.Cnf.Value.NumOfEncryptedBlocks
        if count > payload.Value.Length || count % AesCbc.blockLength <> 0 || (blocks <> 0 && count < 16) then
            Core.decodeError (EncryptionFailed {
                FieldId = payload.Id
                Message = "Mode 5 encrypted payload must contain complete 16-byte blocks within the payload."
            })
        else
            decoder {
                let! encrypted, suffix =
                    Core.parseValue
                        (parser {
                            let! encrypted = parseField "Mode 5 encrypted data" (take count)
                            let! suffix = parseField "Mode 5 clear suffix" takeAll
                            return encrypted, suffix
                        })
                        payload
                return {
                    Frame = { Device = header.Device; AccessNumber = header.Acc }
                    Payload = payload
                    Cipher = encrypted
                    Suffix = suffix
                }
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
                let! context = Mode5SecurityContext.create input.Context.Device input.Context.AccessNumber.Value resolver
                let! plain = AesCbc.decrypt context.Key context.Iv input.Encrypted.Value 0 count
                let! () =
                    if plain.Span[0] <> 0x2Fuy || plain.Span[1] <> 0x2Fuy then
                        Error (EncryptionError.create "Mode 5 decryption verification failed.")
                    else Ok ()

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
            Core.decodePassed input.Origin
        else
            match unprotect resolver input with
            | Error error ->
                Core.decodeError (EncryptionFailed { FieldId = input.Origin.Id; Message = EncryptionError.value error })
            | Ok plain ->
                Core.createDerivedSource
                    "Decrypted APL Data"
                    (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                    true
                    input.Origin
                    plain
