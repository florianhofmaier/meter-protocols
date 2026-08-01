namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.Protection
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
        |> Result.mapError EncryptionError

type Mode5SecurityContext =
    {
        Key: Secret128
        Iv: AesCbCIv
    }

module Mode5SecurityContext =

    let private validateMeterAddress meterAddress=
        match DeviceIdentification.fromRaw meterAddress with
        | Failed (failures, _) ->
            Error (InvalidFrameStructure failures)

        | Passed (validAddress, _) ->
            Ok validAddress

    let private resolveKey
        (resolver: IExternalSecurityContextResolver)
        (meterAddress: DeviceIdentification)=

        match resolver.ResolveMode5Context meterAddress with
        | None ->
            Error (UnprotectionIssue.EncryptionError KeyUnavailable)

        | Some value ->
            Ok value.Key

    let create meterAddress accessNum keyResolver =
        result {
            let accessNumber = AccessNumberRaw.value accessNum
            let! iv = Mode5Iv.create meterAddress accessNumber
            let! validMeterAddress = validateMeterAddress meterAddress
            let! key = resolveKey keyResolver validMeterAddress

            return {
                Key = key
                Iv = iv
            }
        }
