namespace Metering.Common.Security.Cryptography

type EncryptionError = private EncryptionError of string

module EncryptionError =

    /// <summary>Wraps a message as an encryption error.</summary>
    let create message = EncryptionError message

    /// <summary>Returns the message stored in an encryption error.</summary>
    let value (EncryptionError message) = message
