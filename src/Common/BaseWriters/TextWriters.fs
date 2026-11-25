module Metering.Common.BaseWriters.TextWriters

open System
open System.Text
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

/// <summary>Writes text using the supplied encoding and its fallback policy, without a preamble, length prefix or terminator. Encoding fallback and capacity failures return errors without changing the buffer.</summary>
let writeText (encoding: Encoding) (value: string) : Writer<unit> =
    ArgumentNullException.ThrowIfNull encoding
    ArgumentNullException.ThrowIfNull value

    fun state ->
        try
            let bytes = encoding.GetBytes value
            writeMemory (ReadOnlyMemory bytes) state
        with :? EncoderFallbackException as error ->
            Error {
                Pos = state.Pos
                Msg = error.Message
                Ctx = [ "writeText" ]
            }
