namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

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
        (oid: Ber.ObjectIdentifier)
        : Validation<MechanismName> =

        let bytes = Ber.ObjectIdentifier.toBytes oid
        let prefix = [| 0x60uy; 0x85uy; 0x74uy; 0x05uy; 0x08uy; 0x02uy |]

        if not (OidValidation.hasPrefixAndSingleId prefix bytes) then
            Validation.error
                "invalid authentication mechanism object identifier"
        else
            match OidValidation.lastByte bytes with
            | 0x00uy -> Validation.ok LowestLevelSecurity
            | 0x01uy -> Validation.ok LowLevelSecurity
            | 0x02uy -> Validation.ok (HighLevelSecurity HighLevelSecurityManufacturerSpecific)
            | 0x03uy -> Validation.ok (HighLevelSecurity HighLevelSecurityMd5)
            | 0x04uy -> Validation.ok (HighLevelSecurity HighLevelSecuritySha1)
            | 0x05uy -> Validation.ok (HighLevelSecurity HighLevelSecurityGmac)
            | 0x06uy -> Validation.ok (HighLevelSecurity HighLevelSecuritySha256)
            | 0x07uy -> Validation.ok (HighLevelSecurity HighLevelSecurityEcdsa)
            | value ->
                Validation.error
                    $"unsupported authentication mechanism id 0x{value:X2}"