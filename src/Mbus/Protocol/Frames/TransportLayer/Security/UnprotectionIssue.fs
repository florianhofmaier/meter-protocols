namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

type UnprotectionIssue =
    | WrongSecurityContext
    | EncryptionFailed