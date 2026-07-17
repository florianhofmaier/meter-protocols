namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Fields.OidValidation

type HlsAuthenticationMechanismName =
    | HighLevelSecurityManufacturerSpecific
    | HighLevelSecurityMd5
    | HighLevelSecuritySha1
    | HighLevelSecurityGmac
    | HighLevelSecuritySha256
    | HighLevelSecurityEcdsa

type MechanismName =
    | LowestLevelSecurity
    | LowLevelSecurity
    | HighLevelSecurity of HlsAuthenticationMechanismName

module MechanismName =

    let validate
        (raw: Field<Ber.ObjectIdentifier>)
        : Validation<MechanismName> =

        let prefix =
            [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x02uy |]

        validator {
            let! id =
                OidValidation.validatePrefixAndSingleId
                    <| prefix
                    <| "authentication mechanism"
                    <| raw

            match id with
            | 0x00uy ->
                return LowestLevelSecurity

            | 0x01uy ->
                return LowLevelSecurity

            | 0x02uy ->
                return HighLevelSecurity HighLevelSecurityManufacturerSpecific

            | 0x03uy ->
                return HighLevelSecurity HighLevelSecurityMd5

            | 0x04uy ->
                return HighLevelSecurity HighLevelSecuritySha1

            | 0x05uy ->
                return HighLevelSecurity HighLevelSecurityGmac

            | 0x06uy ->
                return HighLevelSecurity HighLevelSecuritySha256

            | 0x07uy ->
                return HighLevelSecurity HighLevelSecurityEcdsa

            | value ->
                return!
                    failed raw $"unsupported authentication mechanism id 0x{value:X2}"
        }