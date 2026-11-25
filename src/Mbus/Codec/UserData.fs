namespace Metering.Mbus.Codec

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.Decoding.Decoders.Core
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Records

module CoreDecoder = Metering.Common.Decoding.Decoders.Core.Decoder

/// Encodes and validates complete response application data independently of telegram framing.
module UserData =

    /// Parses and validates the complete APL record sequence, including optional manufacturer data.
    /// The input may exceed one telegram and must remain stable while returned values reference it.
    let decode (bytes: ReadOnlyMemory<byte>) : DecodeResult<RspUdData> =
        Decoder.run
            "M-Bus user data"
            (fun source ->
                decoder {
                    let! raw = CoreDecoder.parse RecordsRaw.parse source
                    return! CoreDecoder.validate (fun field -> RspUdData.validate field.Value) raw
                })
            bytes

    /// Encodes records into independent bytes without imposing a telegram size limit on their sum.
    let encode (records: DataRecordRsp seq) =
        // EN 13757-3:2025, 6.3/Table 5 and 6.4: at most eleven DIB bytes,
        // 257 bytes for a text VIB and 192 for the largest supported value (LVAR + text).
        let recordCapacity = 11 + 257 + 192

        let chunks = ResizeArray<byte[]>()
        use iterator = records.GetEnumerator()

        let rec append () =
            if iterator.MoveNext() then
                match Writer.run recordCapacity (DataRecordRsp.write iterator.Current) with
                | Error error -> Error error
                | Ok bytes ->
                    chunks.Add bytes
                    append ()
            else
                Ok(Array.concat chunks)

        append ()
