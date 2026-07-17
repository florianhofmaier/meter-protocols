namespace Metering.Dlms.Protocol.Security.ProtectedApdus

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser

type InvocationCounter =
    private InvocationCounter of uint32

module InvocationCounter =

    let value (InvocationCounter value) =
        value

    let toBytes (InvocationCounter value) =
        let bytes = Array.zeroCreate<byte> 4
        BinaryPrimitives.WriteUInt32BigEndian(bytes.AsSpan(), value)
        ReadOnlyMemory bytes

    let parse : Parser<Field<InvocationCounter>> =
        parseField "invocation-counter" parseU32BigEndian
        |>> Field.map InvocationCounter