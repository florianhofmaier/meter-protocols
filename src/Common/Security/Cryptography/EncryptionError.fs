namespace Metering.Common.Security.Cryptography

type EncryptionError =
    | InvalidKeyLength of int
    | InvalidNonceLength of int
    | InvalidTagLength of int
    | AuthenticationFailed
    | CryptographicFailure of string