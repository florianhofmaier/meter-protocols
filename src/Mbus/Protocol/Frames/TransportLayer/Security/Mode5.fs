namespace Metering.Mbus.Protocol.Frames.TransportLayer.Security

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ParserSource
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Security.Cryptography.AesCbc
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.Protection
open Metering.Mbus.Protocol.Frames.TransportLayer

module Mode5 =

    let private blockLength = 16
    let private aesCheck = 0x2Fuy

    let private decrypt
        (ctx: Mode5SecurityContext)
        (bytes: Field<ReadOnlyMemory<byte>>)
        (count: int)
        : Parser<AplDataExpanded> =

        if count = 0 then
            parser { return Unprotected bytes }

        else
            match AesCbc.decrypt ctx.Key ctx.Iv bytes.Value 0 count with
            | Error err ->
                parser {
                    return
                        Protected {
                            Bytes = bytes.Value
                            Failure = UnprotectionIssue.EncryptionError err
                        }
                }

            | Ok plain
                when plain.Length < 2
                     || plain.Span[0] <> aesCheck
                     || plain.Span[1] <> aesCheck ->
                parser {
                    return
                        Protected {
                            Bytes = bytes.Value
                            Failure = EncryptionVerificationFailed
                        }
                }

            | Ok plain ->
                parser {
                    let! source =
                        createDerived
                            "Decrypted APL Data"
                            (SourceTransform.Decrypt "M-Bus security mode 5 AES-CBC-128")
                            true
                            bytes
                            plain

                    return Unprotected source
                }

    let expandLongHeader
        (header: LongHeaderMode5Raw)
        (ctx: Mode5SecurityContext)
        (payload: Field<ReadOnlyMemory<byte>>)
        : Parser<AplDataExpanded> =

        let count = LongHeaderMode5Raw.numberOfEncryptedBytes header
        decrypt ctx payload count
