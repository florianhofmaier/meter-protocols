namespace Metering.Common.Security.Cryptography

type EncryptionError =
    | KeyUnavailable
    | InvalidKeyLength of int
    | InvalidNonceLength of int
    | InvalidInitializationVectorLength of int
    | InvalidTagLength of int
    | AuthenticationFailed
    | CryptographicFailure of string
