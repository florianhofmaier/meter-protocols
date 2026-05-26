namespace Metering.Dlms.Protocol.Acse.Aarq

type AarqTag =
    | ProtocolVersion = 0x80uy
    | SenderAcseRequirements = 0x8Auy
    | MechanismName = 0x8Buy
    | ImplementationInformation = 0x9Duy
    | ApplicationContextName = 0xA1uy

    | CalledApTitle = 0xA2uy
    | CalledAeQualifier = 0xA3uy
    | CalledApInvocationId = 0xA4uy
    | CalledAeInvocationId = 0xA5uy

    | CallingApTitle = 0xA6uy
    | CallingAeQualifier = 0xA7uy
    | CallingApInvocationId = 0xA8uy
    | CallingAeInvocationId = 0xA9uy

    | CallingAuthenticationValue = 0xACuy
    | UserInformation = 0xBEuy