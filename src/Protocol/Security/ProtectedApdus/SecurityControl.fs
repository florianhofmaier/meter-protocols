namespace Metering.Dlms.Protocol.Security.ProtectedApdus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type SecurityControlRaw =
    private SecurityControlRaw of uint8

module SecurityControlRaw =

    let value (SecurityControlRaw value) =
        value

    let parse : Parser<ParsedField<SecurityControlRaw>> =
        parseField "security-control" parseU8
        |>> ParsedField.map SecurityControlRaw

type ProtectedApduKind =
    | ServiceSpecificGlobal
    | ServiceSpecificDedicated
    | GeneralGlobal
    | GeneralDedicated
    | GeneralCiphering

type SecuritySuiteId =
    | Suite0
    | Suite1
    | Suite2

module SecuritySuiteId =

    let value suite =
        match suite with
        | Suite0 -> 0uy
        | Suite1 -> 1uy
        | Suite2 -> 2uy

    let fromParsed
        (field: ParsedField<SecurityControlRaw>)
        : Validation<SecuritySuiteId> =

        let value = SecurityControlRaw.value field.Value

        match value &&& 0x0Fuy with
        | 0uy -> passed Suite0
        | 1uy -> passed Suite1
        | 2uy -> passed Suite2
        | other ->
            failed field $"unsupported Security_Suite_Id {other}"

type ProtectionMode =
    | NoProtection
    | AuthenticationOnly
    | EncryptionOnly
    | AuthenticatedEncryption

type KeySet =
    | Unicast
    | Broadcast

type SecurityControl =
    {
        Raw : SecurityControlRaw
        SecuritySuiteId : SecuritySuiteId
        ProtectionMode : ProtectionMode
        KeySet : KeySet
        CompressionApplied : bool
    }

module SecurityControl =

    let toByte control =
        SecurityControlRaw.value control.Raw

    type private Mask =
        | AuthenticationApplied = 0x10uy
        | EncryptionApplied = 0x20uy
        | KeySet = 0x40uy
        | CompressionApplied = 0x80uy

    let private isApplied
        (field: ParsedField<SecurityControlRaw>)
        (mask: Mask)
        : bool =
        SecurityControlRaw.value field.Value &&& byte mask <> 0uy

    let private protectionMode authenticationApplied encryptionApplied =
        match encryptionApplied, authenticationApplied with
        | false, false -> NoProtection
        | false, true  -> AuthenticationOnly
        | true,  false -> EncryptionOnly
        | true,  true  -> AuthenticatedEncryption


    let private keySet isKeySet =
        if isKeySet then Broadcast else Unicast

    let fromParsed
        (apduKind: ProtectedApduKind)
        (field: ParsedField<SecurityControlRaw>)
        : Validation<SecurityControl> =

        validator {
            let! securitySuiteId =
                SecuritySuiteId.fromParsed field

            let authenticationApplied =
                isApplied field Mask.AuthenticationApplied

            let encryptionApplied =
                isApplied field Mask.EncryptionApplied

            let keySetApplied =
                isApplied field Mask.KeySet

            let compressionApplied =
                isApplied field Mask.CompressionApplied

            let protection =
                protectionMode authenticationApplied encryptionApplied

            let keySet =
                keySet keySetApplied

            do!
                match apduKind, keySetApplied with
                | ServiceSpecificDedicated, true
                | GeneralDedicated, true
                | GeneralCiphering, true ->
                    failed field "Key_Set bit shall be 0 for service-specific dedicated ciphering, general-ded-ciphering and general-ciphering APDUs"

                | _ ->
                    passed ()

            do!
                match apduKind, compressionApplied with
                | ServiceSpecificGlobal, true
                | ServiceSpecificDedicated, true ->
                    failed field "Compression bit shall not be set for service-specific glo/ded ciphering APDUs"

                | _ ->
                    passed ()
            do!
                match protection with
                | NoProtection ->
                    info field "Security Control has neither authentication nor encryption applied; this is a no-protection combination"

                | _ ->
                    passed ()

            return {
                Raw = field.Value
                SecuritySuiteId = securitySuiteId
                ProtectionMode = protection
                KeySet = keySet
                CompressionApplied = compressionApplied
            }
        }