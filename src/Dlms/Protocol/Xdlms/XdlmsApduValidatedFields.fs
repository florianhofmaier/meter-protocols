namespace Metering.Dlms.Protocol.Xdlms

open Metering.Dlms.Protocol.Xdlms.InitiateRequest

type XdlmsApduValidatedFields =
    | InitiateRequest of InitiateRequestValidatedFields
