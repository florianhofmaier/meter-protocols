module Metering.Common.Parsers.BaseParsers

open System
open Metering.Common.Parsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ErrorHandling

let getOffset: Parser<int> =
    fun st -> ok st.Off st

let parseByte : Parser<uint8> =
    fun st ->
        if st.Off >= st.Buf.Length then errBufOverflow st
        else ok st.Buf.Span[st.Off] { st with Off = st.Off + 1 }

let skipByte : Parser<unit> =
    fun st ->
        if st.Off >= st.Buf.Length then
            errBufOverflow st
        else
            ok () { st with Off = st.Off + 1 }

let skipBytes n : Parser<unit> =
    fun st ->
        if n < 0 then
            err st $"cannot skip negative byte count {n}"
        elif st.Off + n > st.Buf.Length then
            errBufOverflow st
        else
            ok () { st with Off = st.Off + n }

let peekByte : Parser<uint8> =
    fun st ->
        if st.Off >= st.Buf.Length then errBufOverflow st
        else ok st.Buf.Span[st.Off] st

let takeMem (n:int) : Parser<ReadOnlyMemory<uint8>> =
    fun st ->
        if st.Off + n > st.Buf.Length then errBufOverflow st
        else
            let mem = st.Buf.Slice(st.Off, n)
            ok mem { st with Off = st.Off + n }

let takeAllMem : Parser<ReadOnlyMemory<uint8>> =
    fun st ->
        let mem = st.Buf.Slice(st.Off)
        ok mem { st with Off = st.Buf.Length }

let expectEnd : Parser<unit> =
    fun st ->
        if st.Off = st.Buf.Length then ok () st
        else err st "expected end of buffer"

let remainder : Parser<int> =
    fun st -> ok (st.Buf.Length - st.Off) st

let runOnSubSlice (n:int) (p: Parser<'a>) : Parser<'a> =
    fun st ->
        if st.Off + n > st.Buf.Length then
            errBufOverflow st
        else
            let subSt =
                {
                    Buf = st.Buf.Slice(st.Off, n)
                    Off = 0
                    BasePos = st.BasePos + st.Off
                    Tree = st.Tree
                    Current = st.Current
                }

            match p subSt with
            | Ok (a, _) ->
                ok a { st with Off = st.Off + n }

            | Error e ->
                Error e

