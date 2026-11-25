namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

/// Opaque wired DLL Link User Data whose length fits the FT 1.2 L-field alongside C and A.
type LinkUserDataBytes = private LinkUserDataBytes of ReadOnlyMemory<byte>

module LinkUserDataBytes =

    /// IEC 60870-5-2, 3.2, p. 14; EN 13757-2, F.2, p. 37: L includes C and A.
    [<Literal>]
    let maxLength = 253

    /// Creates Link User Data only if its length fits the wired DLL; the memory is not copied.
    let create (bytes: ReadOnlyMemory<byte>) : Result<LinkUserDataBytes, string> =
        if bytes.Length > maxLength then
            Error $"Link User Data exceeds the FT 1.2 limit of {maxLength} bytes: {bytes.Length}"
        else
            Ok(LinkUserDataBytes bytes)

    /// Returns the opaque byte range. Its length is fixed by the validated value.
    let bytes (LinkUserDataBytes bytes) = bytes

    /// Validates raw Link User Data while retaining its field identity and source span.
    let validate (raw: Field<ReadOnlyMemory<byte>>) : Validation<Field<LinkUserDataBytes>> =
        match create raw.Value with
        | Ok value -> raw |> Field.withValue value |> passed
        | Error message -> failed raw message

    /// Writes the bounded opaque Link User Data.
    let write data =
        Metering.Common.BaseWriters.BinaryWriters.writeMemory (bytes data)
