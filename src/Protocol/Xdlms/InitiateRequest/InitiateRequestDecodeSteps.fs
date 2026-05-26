module Metering.Dlms.Protocol.Xdlms.InitiateRequest.InitiateRequestDecodeSteps

open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Xdlms
open Metering.Dlms.Protocol.Xdlms.Common

let private initiateRequestSteps : XdlmsApduDecodeSteps<InitiateRequestRaw, InitiateRequestValidatedFields> =
    {
        Parse =
            InitiateRequestRaw.parse

        Decode =
            fun _ raw ->
                raw
                |> InitiateRequestValidatedFields.fromRaw
    }

let gloInitiateRequestSteps : XdlmsApduDecodeSteps<Axdr.OctetString, InitiateRequestValidatedFields> =
     {
         Parse =
             InitiateRequestRaw.parse


     }