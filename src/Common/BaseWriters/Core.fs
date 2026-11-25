module Metering.Common.BaseWriters.Core

open System

type WState = { Buf: byte[]; Pos: int }

module WState =
    /// <summary>Creates a writer state with a zero-filled 256-byte buffer and position zero.</summary>
    let create () = {
        Buf = Array.zeroCreate<byte> 256
        Pos = 0
    }

    /// <summary>Returns the state's backing buffer without copying it.</summary>
    let buf (st: WState) = st.Buf
    /// <summary>Returns the current write position in the backing buffer.</summary>
    let pos (st: WState) = st.Pos

type WError = {
    Pos: int
    Msg: string
    Ctx: string list
}

type Writer<'T> = WState -> Result<'T * WState, WError>

/// <summary>Creates an error at the state's current position with the supplied message and an empty context list.</summary>
let err (st: WState) msg =
    Error { Pos = st.Pos; Msg = msg; Ctx = [] }

module Writer =
    /// <summary>Executes the writer once in a fresh buffer of the specified capacity and returns an independent copy of bytes from zero to the final position. After backpatching, restore the end position before returning. Invalid capacity or final state and writer failures return an error; partial output is not returned.</summary>
    let run capacity (write: Writer<unit>) : Result<byte[], WError> =
        if capacity < 0 then
            Error {
                Pos = 0
                Msg = "Writer capacity must not be negative"
                Ctx = [ "run" ]
            }
        else
            let initial = {
                Buf = Array.zeroCreate<byte> capacity
                Pos = 0
            }

            match write initial with
            | Error error -> Error error
            | Ok((), final) ->
                if isNull final.Buf || final.Pos < 0 || final.Pos > final.Buf.Length then
                    Error {
                        Pos = final.Pos
                        Msg = "Writer returned an invalid buffer or position"
                        Ctx = [ "run" ]
                    }
                else
                    Ok(ReadOnlyMemory<byte>(final.Buf, 0, final.Pos).ToArray())

    /// <summary>Creates a writer that returns the supplied value without changing the writer state.</summary>
    let ret x : Writer<'T> = fun st -> Ok(x, st)

    /// <summary>Passes a successful value and updated state to the next writer, stopping on error.</summary>
    let bind (m: Writer<'T>) (f: 'T -> Writer<'U>) : Writer<'U> =
        fun st ->
            match m st with
            | Ok(x, st') -> f x st'
            | Error e -> Error e

module WriterBuilderImpl =
    /// <summary>Creates the body writer and disposes the resource after that writer executes, including when execution throws.</summary>
    let using (disposable: 'D) (body: 'D -> Writer<'U>) : Writer<'U> when 'D :> IDisposable =
        let body' = body disposable

        fun st ->
            try
                body' st
            finally
                disposable.Dispose()

type WriterBuilder() =
    /// <summary>Returns a value without changing the writer state.</summary>
    member _.Return(x) = Writer.ret x
    /// <summary>Uses an existing writer as the result of the computation expression.</summary>
    member _.ReturnFrom(m: Writer<'T>) = m
    /// <summary>Returns unit without changing the writer state.</summary>
    member _.Zero() = Writer.ret ()
    /// <summary>Continues writing after success with the updated state, stopping on error.</summary>
    member _.Bind(m: Writer<'T>, f: 'T -> Writer<'U>) = Writer.bind m f
    /// <summary>Runs two writers in sequence, discarding the first writer's unit result and stopping on error.</summary>
    member _.Combine(m1: Writer<unit>, m2: Writer<'T>) = Writer.bind m1 (fun () -> m2)
    /// <summary>Defers construction of a writer until that writer is executed.</summary>
    member _.Delay(f: unit -> Writer<'T>) = fun st -> f () st

    /// <summary>Writes each element in enumeration order, stopping on error and disposing the enumerator after execution.</summary>
    member this.For(sequence: seq<'T>, body: 'T -> Writer<unit>) =
        WriterBuilderImpl.using
            (sequence.GetEnumerator())
            (fun (enum: System.Collections.Generic.IEnumerator<'T>) ->
                this.While((fun () -> enum.MoveNext()), this.Delay(fun () -> body enum.Current)))

    /// <summary>Repeats the body while the guard holds, threading writer state through iterations and stopping on error.</summary>
    member _.While(guard, body) =
        let rec loop () =
            if guard () then
                Writer.bind body (fun () -> loop ())
            else
                Writer.ret ()

        loop ()

    /// <summary>Executes a writer with a disposable resource and disposes it after execution, including when execution throws.</summary>
    member _.Using(disposable: 'D, body: 'D -> Writer<'U>) = WriterBuilderImpl.using disposable body

/// <summary>Builds writer computation expressions that thread buffer state and stop on errors.</summary>
let writer = WriterBuilder()

/// <summary>Creates a writer that fails at the current position with the supplied message.</summary>
let writerError msg : Writer<'a> = fun st -> err st msg

/// <summary>Returns a view of the specified backing-buffer range without moving the write position; an invalid range throws.</summary>
let getMem start len : Writer<ReadOnlyMemory<byte>> =
    fun st -> Ok(ReadOnlyMemory<byte>(st.Buf, start, len), st)

/// <summary>Returns the current write position without changing writer state.</summary>
let getPos: Writer<int> = fun st -> Ok(st.Pos, st)

/// <summary>Moves the write position within the backing buffer, including its end, or returns an error when out of bounds.</summary>
let seek (pos: int) : Writer<unit> =
    fun st ->
        if pos < 0 || pos > st.Buf.Length then
            Error {
                Pos = st.Pos
                Msg = $"seek out of bounds: {pos}"
                Ctx = [ "seek" ]
            }
        else
            Ok((), { st with Pos = pos })

let inline internal ensureSpace (st: WState) n : Result<WState, WError> =
    if isNull st.Buf || st.Pos < 0 || st.Pos > st.Buf.Length then
        err st "invalid writer buffer or position"
    elif n < 0 || n > st.Buf.Length - st.Pos then
        err st $"not enough space in buffer to write {n} bytes"
    else
        Ok st

let inline internal checkedWrite (n: int) (writeAt: WState -> unit) : Writer<unit> =
    fun st ->
        ensureSpace st n
        |> Result.map (fun st ->
            writeAt st
            (), { st with Pos = st.Pos + n })

let inline internal writeBytesLittleEndian (byteCount: int) (value: uint64) : Writer<unit> =
    checkedWrite byteCount (fun st ->
        for i = 0 to byteCount - 1 do
            st.Buf[st.Pos + i] <- byte ((value >>> (8 * i)) &&& 0xFFUL))
