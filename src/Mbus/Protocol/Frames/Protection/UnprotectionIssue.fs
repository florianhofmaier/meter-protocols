namespace Metering.Mbus.Protocol.Frames.Protection

open Metering.Common.Decoding.Validators.Core
open Metering.Common.Security.Cryptography

type UnprotectionIssue =
    | SecurityContextNotUsable
    | EncryptionVerificationFailed
    | InvalidFrameStructure of Failures
    | EncryptionError of EncryptionError
