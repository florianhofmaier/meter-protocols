namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Security.Cryptography
open Metering.Mbus.Protocol.Frames.TransportLayer

[<RequireQualifiedAccess>]
type MbusProtectionInput =
    | Plain of Field<ReadOnlyMemory<byte>>
    | Mode5 of Mode5ProtectionInput

module MbusProtectionInput =

    /// Selects and prepares the operation from validated TPL fields, without keys.
    let decode (tpl: Tpl) : Decoder<MbusProtectionInput> =
        let payload = Tpl.aplData tpl
        match tpl with
        | Tpl.LongHeader { Header = { Value = LongHeader.Mode5 header } } ->
            decoder {
                let! input = Mode5.decodeInput header payload
                return MbusProtectionInput.Mode5 input
            }
        | _ -> Core.decodePassed (MbusProtectionInput.Plain payload)

module MbusProtection =

    let unprotect resolver input : Result<ReadOnlyMemory<byte>, EncryptionError> =
        match input with
        | MbusProtectionInput.Plain bytes -> Ok bytes.Value
        | MbusProtectionInput.Mode5 input -> Mode5.unprotect resolver input

    let decode resolver input : Decoder<Field<ReadOnlyMemory<byte>>> =
        match input with
        | MbusProtectionInput.Plain bytes -> Core.decodePassed bytes
        | MbusProtectionInput.Mode5 input -> Mode5.decode resolver input
