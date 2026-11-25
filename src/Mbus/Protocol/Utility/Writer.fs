module Metering.Mbus.Protocol.Utility.Writer

open Metering.Common.BaseWriters.Core

/// Executes an FT 1.2 frame writer with room for 255 user-data bytes and six framing bytes.
/// IEC 60870-5-2, 3.2, p. 14. Only the written bytes are returned; the writer executes once.
let runFt12Writer (write: Writer<unit>) : Result<byte[], WError> = Writer.run (255 + 6) write
