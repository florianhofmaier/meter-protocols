namespace Metering.Common.Security.Cryptography

type EncryptionError =
    private EncryptionError of string

module EncryptionError =

    let create message =
        EncryptionError message

    let value (EncryptionError message) =
        message