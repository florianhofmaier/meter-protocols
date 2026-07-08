namespace Metering.Mbus.Protocol.Records.Values

open System
open System.Buffers.Binary
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core

module private Utility =

    let checkValueLength
        (bytes: ParsedField<ReadOnlyMemory<uint8>>)
        (expected: int)
        : Validation<unit> =

        validator {
            if bytes.Value.Length <> expected then
                return!
                    failed
                        bytes
                        $"Expected {expected} bytes, but got {bytes.Value.Length} bytes."
            else
                return ()
        }

    let convert
        (bytes: ParsedField<ReadOnlyMemory<uint8>>)
        (len: int)
        (f: ReadOnlyMemory<uint8> -> 'a)
        : Validation<'a> =
        validator {
            do! checkValueLength bytes len
            return f bytes.Value
        }

    let convertWithValidator
        (bytes: ParsedField<ReadOnlyMemory<uint8>>)
        (len: int)
        (f: ParsedField<ReadOnlyMemory<uint8>> -> Validation<'a>)
        : Validation<'a> =
        validator {
            do! checkValueLength bytes len
            return! f bytes
        }

module Integer8Bit =

    let fromBytes bytes: Validation<int8> =
        Utility.convert bytes 1 (fun b -> int8 b.Span[0])

module Integer16Bit =

    let fromBytes bytes: Validation<int16> =
        Utility.convert bytes 2 (fun b -> BinaryPrimitives.ReadInt16LittleEndian b.Span)

module Integer24Bit =

    let private read24BitLittleEndian (bytes: ReadOnlyMemory<byte>) : int32 =
        let span = bytes.Span
        let value =
            uint32 span[0] <<< 16
            ||| uint32 span[1] <<< 8
            ||| uint32 span[2]
        int32 value

    let fromBytes bytes: Validation<int32> =
        Utility.convert bytes 3 read24BitLittleEndian

module Integer32Bit =

    let fromBytes bytes: Validation<int32> =
        Utility.convert bytes 4 (fun b -> BinaryPrimitives.ReadInt32LittleEndian b.Span)

module Integer48Bit =

    let private read48BitLittleEndian (bytes: ReadOnlyMemory<byte>) : int64 =
        let span = bytes.Span
        let value =
            uint64 span[0] <<< 40
            ||| uint64 span[1] <<< 32
            ||| uint64 span[2] <<< 24
            ||| uint64 span[3] <<< 16
            ||| uint64 span[4] <<< 8
            ||| uint64 span[5]
        int64 value

    let fromBytes bytes: Validation<int64> =
        Utility.convert bytes 6 read48BitLittleEndian

module Integer64Bit =

    let fromBytes bytes: Validation<int64> =
        Utility.convert bytes 8 (fun b -> BinaryPrimitives.ReadInt64LittleEndian b.Span)

module Real32Bit =

    let fromBytes bytes: Validation<float32> =
        Utility.convert bytes 4 (fun b -> BinaryPrimitives.ReadSingleLittleEndian b.Span)


module Bcd =

    let private tryByteToUInt64 (b: byte) : uint64 option =
        let hi = uint64 ((b >>> 4) &&& 0x0Fuy)
        let lo = uint64 (b &&& 0x0Fuy)

        if hi <= 9UL && lo <= 9UL then
            Some (hi * 10UL + lo)
        else
            None

    let private tryFromLittleEndian
        (bytes: ReadOnlyMemory<byte>)
        : uint64 option =

        let rec loop (remaining: ReadOnlyMemory<byte>) (acc: uint64) (factor: uint64) =
            if remaining.Length = 0 then
                Some acc
            else
                match tryByteToUInt64 remaining.Span[0] with
                | Some part ->
                    loop (remaining.Slice(1)) (acc + part * factor) (factor * 100UL)
                | None ->
                    None

        loop bytes 0UL 1UL

    let fromBytes bytes: Validation<uint64> =
        validator {
            let result = tryFromLittleEndian bytes.Value

            match result with
            | Some value ->
                return value

            | None ->
                return!
                    failed
                    <| bytes
                    <| $"Invalid BCD value: {Convert.ToHexString bytes.Value.Span}"
        }

module Bcd2Digit =

    let fromBytes bytes: Validation<uint8> =
        Utility.convertWithValidator bytes 1 (fun b -> Bcd.fromBytes b |> map uint8)

module Bcd4Digit =

    let fromBytes bytes: Validation<uint16> =
        Utility.convertWithValidator bytes 2 (fun b -> Bcd.fromBytes b |> map uint16)

module Bcd6Digit =

    let fromBytes bytes: Validation<uint32> =
        Utility.convertWithValidator bytes 3 (fun b -> Bcd.fromBytes b |> map uint32)

module Bcd8Digit =

    let fromBytes bytes: Validation<uint32> =
        Utility.convertWithValidator bytes 4 (fun b -> Bcd.fromBytes b |> map uint32)

module Bcd12Digit =

    let fromBytes bytes: Validation<uint64> =
        Utility.convertWithValidator bytes 6 Bcd.fromBytes

module Text =

    let fromBytes bytes: Validation<string> =
        Utility.convert
            bytes
            bytes.Value.Length
            (fun b -> System.Text.Encoding.Latin1.GetString b.Span)