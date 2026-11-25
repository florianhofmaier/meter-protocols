namespace Metering.Mbus.Codec

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.Security.Cryptography
open Metering.Common.Utility.Result
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Messages
open Metering.Mbus.Protocol.Utility.Writer

/// Transport fields supplied for each outgoing message.
type EncodingContext = { AccessNumber: AccessNumber }

type Mode5EncodingContext = { SecondaryAddress: MeterAddress }

[<RequireQualifiedAccess>]
type EncodingOptions =
    | Plain
    | Mode5 of Mode5EncodingContext

[<RequireQualifiedAccess>]
type EncodeError =
    | Writing of WError
    | Protection of EncryptionError
    | Unsupported of string

    member error.Message =
        match error with
        | EncodeError.Writing error -> error.Msg
        | EncodeError.Protection error -> EncryptionError.value error
        | EncodeError.Unsupported message -> message

module Encoder =

    let private run writer =
        runFt12Writer writer |> Result.mapError EncodeError.Writing

    /// Creates an encoder that holds its protection policy and external key resolver.
    /// Access comes from the per-message encoding context; Mode 5 also requires a secondary address.
    /// Mode 5 protects complete response records and ChangePrimaryAddress.
    /// Link services without a TPL header retain their defined unprotected frame format.
    let create
        options
        (resolver: IExternalSecurityContextResolver)
        : EncodingContext -> Message -> Result<byte[], EncodeError> =
        fun context message ->
            let prepare meter plainHeader protectedHeader payloadWriter =
                result {
                    let! bytes =
                        Writer.run LinkUserDataBytes.maxLength payloadWriter
                        |> Result.mapError EncodeError.Writing

                    let payload = ReadOnlyMemory bytes

                    match options with
                    | EncodingOptions.Plain ->
                        return plainHeader meter ConfigurationFieldMode0.standardData, payload
                    | EncodingOptions.Mode5 mode5 ->
                        // CI + long TPL header use 13 bytes; 240 bytes remain for AES check and records.
                        let protectedResult =
                            try
                                if bytes.Length > 238 then
                                    Error(
                                        EncodeError.Unsupported
                                            "Mode 5 record data plus AES check exceeds the wired FT 1.2 payload limit"
                                    )
                                else
                                    Mode5.protect
                                        resolver
                                        mode5.SecondaryAddress
                                        context.AccessNumber
                                        payload
                                    |> Result.mapError EncodeError.Protection
                            finally
                                System.Security.Cryptography.CryptographicOperations.ZeroMemory(
                                    bytes.AsSpan()
                                )

                        let! protectedBytes = protectedResult

                        let configuration: ConfigurationFieldMode5 = {
                            HopCounter = HopCounter.Zero
                            RepeaterAccess = RepeaterAccess.NotSet
                            ContentOfMsg = ContentOfMessage.StandardData
                            NumOfEncryptedBlocks =
                                NumberOfEncryptedBlocks.tryCreate 15 |> Option.get
                            Synchronized = Synchronization.NotSynchronized
                            Accessibility = Accessibility.NoAccessWindow
                        }

                        return protectedHeader mode5.SecondaryAddress configuration, protectedBytes
                }

            match message with
            | Message.PrimaryStationMessage(PrimaryStationMessage.ChangePrimaryAddress change) ->
                match options with
                | EncodingOptions.Plain -> run (ChangePrimaryAddress.write change)
                | EncodingOptions.Mode5 mode5 ->
                    result {
                        let! header, payload =
                            prepare
                                mode5.SecondaryAddress
                                (fun meter ->
                                    LongHeader.writeCommandMode0 meter context.AccessNumber)
                                (fun meter ->
                                    LongHeader.writeCommandMode5 meter context.AccessNumber)
                                (ChangePrimaryAddress.writePayload change)

                        return! run (ChangePrimaryAddress.writeWithHeader header payload change)
                    }
            | Message.PrimaryStationMessage primary ->
                // EN 13757-2, 5.7.3; EN 13757-7, Table 2: these services have no TPL header.
                run (PrimaryStationMessage.write primary)
            | Message.SecondaryStationMessage(SecondaryStationMessage.ResponseUserData response) ->
                match options, response.MoreFollows with
                | EncodingOptions.Mode5 _, true ->
                    Error(
                        EncodeError.Unsupported
                            "Mode 5 multi-datagram responses are not implemented"
                    )
                | _ ->
                    result {
                        let! header, payload =
                            prepare
                                response.MeterAddress
                                (fun meter ->
                                    LongHeader.writeMode0
                                        meter
                                        context.AccessNumber
                                        response.Status)
                                (fun meter ->
                                    LongHeader.writeMode5
                                        meter
                                        context.AccessNumber
                                        response.Status)
                                (ResponseUserData.writePayload response)

                        return! run (ResponseUserData.write header payload response)
                    }
            | Message.SecondaryStationMessage(SecondaryStationMessage.ResponseSelectedApplication response) ->
                match options with
                | EncodingOptions.Plain ->
                    run (ResponseSelectedApplication.write context.AccessNumber response)
                | EncodingOptions.Mode5 _ ->
                    Error(
                        EncodeError.Unsupported
                            "Mode 5 selected-application responses are not implemented"
                    )
            | Message.SecondaryStationMessage(SecondaryStationMessage.ResponseAlarms response) ->
                match options with
                | EncodingOptions.Mode5 _ ->
                    Error(EncodeError.Unsupported "Mode 5 alarm-bit encoding is not implemented")
                | EncodingOptions.Plain ->
                    result {
                        let! header, payload =
                            prepare
                                response.MeterAddress
                                (fun meter ->
                                    LongHeader.writeMode0
                                        meter
                                        context.AccessNumber
                                        response.Status)
                                (fun meter ->
                                    LongHeader.writeMode5
                                        meter
                                        context.AccessNumber
                                        response.Status)
                                (ResponseAlarms.writePayload response)

                        return! run (ResponseAlarms.write header payload response)
                    }
