module Mbus.BaseWriters.Core

open System

type WState = { Buf: byte[]; Pos: int }

module WState =
    let create () =
        { Buf = Array.zeroCreate<byte> 256; Pos = 0 }
    let buf (st: WState) = st.Buf
    let pos (st: WState) = st.Pos

type WError = { Pos: int; Msg: string; Ctx: string list }

type Writer<'T> = WState -> Result<'T * WState, WError>

let err (st: WState) msg = Error { Pos = st.Pos; Msg = msg; Ctx = [] }

module Writer =
    let ret x : Writer<'T> = fun st -> Ok (x, st)
    let bind (m: Writer<'T>) (f: 'T -> Writer<'U>) : Writer<'U> =
        fun st ->
            match m st with
            | Ok (x, st') -> f x st'
            | Error e -> Error e

module WriterBuilderImpl =
    let using (disposable: 'D) (body: 'D -> Writer<'U>) : Writer<'U> when 'D :> IDisposable =
        let body' = body disposable
        fun st ->
            try
                body' st
            finally
                disposable.Dispose()

type WriterBuilder() =
    member _.Return(x) = Writer.ret x
    member _.ReturnFrom(m: Writer<'T>) = m
    member _.Zero() = Writer.ret ()
    member _.Bind(m: Writer<'T>, f: 'T -> Writer<'U>) = Writer.bind m f
    member _.Combine(m1: Writer<unit>, m2: Writer<'T>) = Writer.bind m1 (fun () -> m2)
    member _.Delay(f: unit -> Writer<'T>) = fun st -> f () st

    member this.For(sequence: seq<'T>, body: 'T -> Writer<unit>) =
        WriterBuilderImpl.using (sequence.GetEnumerator()) (fun (enum: System.Collections.Generic.IEnumerator<'T>) ->
            this.While((fun () -> enum.MoveNext()),
                this.Delay(fun () -> body enum.Current)))

    member _.While(guard, body) =
        let rec loop () =
            if guard() then
                Writer.bind body (fun () -> loop())
            else
                Writer.ret ()
        loop ()

    member _.Using(disposable: 'D, body: 'D -> Writer<'U>) =
        WriterBuilderImpl.using disposable body

let writer = WriterBuilder()

let writerError msg : Writer<'a> =
    fun st -> err st msg

let getMem start len: Writer<ReadOnlyMemory<byte>> =
    fun st -> Ok (ReadOnlyMemory<byte>(st.Buf, start, len), st)

let getPos : Writer<int> =
    fun st -> Ok (st.Pos, st)

let seek (pos: int) : Writer<unit> =
    fun st ->
        if pos < 0 || pos > st.Buf.Length then
             Error { Pos = st.Pos; Msg = $"seek out of bounds: {pos}"; Ctx = ["seek"] }
        else
             Ok ((), { st with Pos = pos })

let inline internal ensureSpace (st: WState) n : Result<WState, WError> =
    if st.Pos + n > st.Buf.Length then
        err st $"not enough space in buffer to write {n} bytes"
    else
        Ok st

let inline internal checkedWrite (n: int) (writeAt: WState -> unit) : Writer<unit> =
    fun st ->
        ensureSpace st n
        |> Result.map (fun st ->
            writeAt st
            (), { st with Pos = st.Pos + n })

let inline internal writeBytes (byteCount: int) (value: uint64) : Writer<unit> =
    checkedWrite byteCount (fun st ->
        for i = 0 to byteCount - 1 do
            st.Buf[st.Pos + i] <- byte ((value >>> (8 * i)) &&& 0xFFUL))