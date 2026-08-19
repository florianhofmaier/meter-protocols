namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer

type Mode5ExternalContext(key: Secret128) =
    member _.Key = key

type IExternalSecurityContextResolver =
    abstract ResolveMode5Context:
        meterIdentity: DeviceIdentification ->
            Mode5ExternalContext option

module Mode5Iv =

    let create (meterAddress: DeviceIdentificationRaw) accessNr =
        let ivBytes = Array.zeroCreate AesCbCIv.length
        let destination = ivBytes.AsSpan()

        ManufacturerRaw.copyTo
            (destination.Slice(0, 2))
            meterAddress.Mfr.Value

        IdNumberRaw.copyTo
            (destination.Slice(2, 4))
            meterAddress.IdNum.Value

        destination[6] <- VersionRaw.value meterAddress.Version.Value
        destination[7] <- DeviceTypeRaw.value meterAddress.DevType.Value
        destination.Slice(8, 8).Fill(accessNr)

        AesCbCIv.create ivBytes

type Mode5SecurityContext =
    {
        Key: Secret128
        Iv: AesCbCIv
    }

module Mode5SecurityContext =

    let private validateMeterAddress meterAddress=
        match DeviceIdentification.fromRaw meterAddress with
        | Failed (failures, _) ->
            Error failures

        | Passed (validAddress, _) ->
            Ok validAddress

    let private resolveKey
        (resolver: IExternalSecurityContextResolver)
        (meterAddress: DeviceIdentification)=

        match resolver.ResolveMode5Context meterAddress with
        | None ->
            "No external security context found for meter address."
            |> EncryptionError.create
            |> Error

        | Some value ->
            Ok value.Key

    let create
        (meterAddress: DeviceIdentificationRaw)
        (accessNum: AccessNumberRaw)
        (keyResolver: IExternalSecurityContextResolver)
        : Result<Mode5SecurityContext, UnprotectionError> =
        result {
            let accessNumber = AccessNumberRaw.value accessNum

            let! iv =
                Mode5Iv.create meterAddress accessNumber
                |> Result.mapError UnprotectionError.Encryption

            let! validMeterAddress =
                validateMeterAddress meterAddress
                |> Result.mapError UnprotectionError.Validation

            let! key =
                resolveKey keyResolver validMeterAddress
                |> Result.mapError UnprotectionError.Encryption

            return {
                Key = key
                Iv = iv
            }
        }
