namespace Metering.Dlms.Protocol.Xdlms

open System
open Metering.Common.Parsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp
open Metering.Dlms.Protocol.Security
open Metering.Dlms.Protocol.Xdlms.InitiateRequest

type DecodeContext =
    {
        SupportingLayer : CommunicationProfileContext
        CipherContext : ExternalCipherContext
    }

type XdlmsApduDecodeSteps =
    {
        Parser :
            Parser<Field<XdlmsApduRaw>>

        Decoder :
            DecodeContext -> Field<XdlmsApduRaw> -> ValidationReport<XdlmsApduValidatedFields>
    }

module XdlmsApduDecodeStepsModule =

    let private initiateRequestDecoder : XdlmsApduDecodeSteps =
        {
            Parser =
                parser {
                    do! Tag.expect XdlmsTag.InitiateRequest
                    return! InitiateRequestRaw.parse |>> Parsed.map XdlmsApduRaw.InitiateRequest
                }

            Decoder =
                fun _ parsed ->
                    parsed
                    Validation.runParsed InitiateRequestValidatedFields.fromRaw parsed
                    |> ValidationReport.map XdlmsApduValidatedFields.InitiateRequest
        }

    let forTag tag =
        match tag with
        | XdlmsTag.InitiateRequest ->
            initiateRequestDecoder

        | XdlmsTag.GloInitiateRequest ->

        | _ -> raise (ArgumentException ())