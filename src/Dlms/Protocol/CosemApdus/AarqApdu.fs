namespace Metering.Dlms.Protocol.CosemApdus

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Acse.Aarq
open Metering.Dlms.Protocol.Acse.Fields
open Metering.Dlms.Protocol.Security
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.InitiateRequest
open Metering.Dlms.Protocol.Xdlms.Security

type AarqApdu =
    {
        Aarq : AarqValidatedFields
        InitiateRequest : InitiateRequestValidatedFields
    }

module AarqApdu =

    let private unprotectUserInformation
        (cipherContext: ExternalCipherContext)
        (userInformation: Field<UserInformation>)
        : Decoder<Field<ReadOnlyMemory<byte>>> =

        decoder {
            let bytes =
                userInformation
                |> Field.map UserInformation.value

            let! tag =
                parseValue Tag.peek<XdlmsTag> bytes

            match tag with
            | XdlmsTag.InitiateRequest ->
                return bytes

            | XdlmsTag.GloInitiateRequest ->
                let! raw =
                    parse GloInitiateRequestRaw.parse bytes

                let! validated =
                    validate GloInitiateRequestValidatedFields.fromRaw raw

                let payload =
                    GloInitiateRequestValidatedFields.protectedApdu validated

                match cipherContext with
                | Global globaleCipherContext ->

                    return! GloCiphering.unprotect globaleCipherContext payload

            | XdlmsTag.DedInitiateRequest ->
                return bytes

            | XdlmsTag.GeneralGloCiphering
            | XdlmsTag.GeneralDedCiphering
            | XdlmsTag.GeneralCiphering ->
                return!
                    EncryptionError.create "General ciphering not supported"
                    |> EncryptionFailed
                    |> decodeError

            | _ ->
                return!
                    validate
                        failed bytes "unknown user information tag"
        }

    let decode
        (bytes : Field<ReadOnlyMemory<byte>>)
        : Decoder<AarqApdu> =

        decoder {
            let! aarqRaw =
                parse AarqRaw.parse bytes

            let! aarqFields =
                validate AarqValidatedFields.fromParsed aarqRaw

            let! initiateRequestRaw =
                aarqFields.UserInformation
                |> unprotectUserInformation

            let! initiateRequest =
                parse InitiateRequestRaw.parse initiateRequestRaw

            let! initiateRequestFields =
                validate InitiateRequestValidatedFields.fromParsed initiateRequest

            return {
                Aarq = aarqFields
                InitiateRequest = initiateRequestFields
            }
        }