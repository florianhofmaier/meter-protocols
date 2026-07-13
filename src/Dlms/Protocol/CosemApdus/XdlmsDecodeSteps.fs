namespace Metering.Dlms.Protocol.CosemApdus

open System
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol.CommunicationProfile.TcpUdpIp
open Metering.Dlms.Protocol.Security
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.InitiateRequest

type DecodeContext =
    {
        SupportingLayer : CommunicationProfileContext
        CipherContext : ExternalCipherContext
    }

type XdlmsDecodeSteps<'a, 'b> =
    {
        Parse : Parser<Parsed<'a>>
        Decode : DecodeContext -> Parsed<'a> -> Validation<'b>
    }

module XdlmsDecodeSteps =

    let private initiateRequest : XdlmsDecodeSteps<InitiateRequestRaw, InitiateRequestValidatedFields> =
        {
            Parse =
                InitiateRequestRaw.parse

            Decode =
                fun _ raw ->
                    raw
                    |> InitiateRequestValidatedFields.fromRaw
        }

    let forTag tag =
        match tag with
        | XdlmsTag.InitiateRequest -> initiateRequest

        | _ -> raise (ArgumentException ())
