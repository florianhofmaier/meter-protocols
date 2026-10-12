namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open System.Buffers.Binary
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer

type Mode5ExternalContext(key: Secret128) =
    member _.Key = key

type IExternalSecurityContextResolver =
    abstract ResolveMode5Context:
        meterIdentity: DeviceIdentification ->
            Mode5ExternalContext option

module Mode5Iv =

    let create (meterAddress: DeviceIdentification) accessNr =
        let ivBytes = Array.zeroCreate AesCbCIv.length
        let destination = ivBytes.AsSpan()

        BinaryPrimitives.WriteUInt16LittleEndian(
            destination.Slice(0, 2), Manufacturer.value meterAddress.Mfr.Value)

        BinaryPrimitives.WriteUInt32LittleEndian(
            destination.Slice(2, 4), IdNumber.toBcd meterAddress.IdNum.Value)

        destination[6] <- Version.value meterAddress.Version.Value
        destination[7] <- DeviceType.value meterAddress.DevType.Value
        destination.Slice(8, 8).Fill(accessNr)

        AesCbCIv.create ivBytes

type Mode5SecurityContext =
    {
        Key: Secret128
        Iv: AesCbCIv
    }

module Mode5SecurityContext =

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
        (meterAddress: DeviceIdentification)
        (accessNum: AccessNumber)
        (keyResolver: IExternalSecurityContextResolver)
        : Result<Mode5SecurityContext, EncryptionError> =
        result {
            let accessNumber = AccessNumber.value accessNum

            let! iv =
                Mode5Iv.create meterAddress accessNumber

            let! key =
                resolveKey keyResolver meterAddress

            return {
                Key = key
                Iv = iv
            }
        }
