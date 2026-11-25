namespace Metering.Common.Decoding.Parsers.Types

open System

[<Struct>]
type SourceId = { Value: int }

module SourceId =

    /// <summary>The identifier used by parser runners for the default root source.</summary>
    let root = { Value = 0 }

    /// <summary>The sentinel identifier for parser errors whose source has not yet been assigned.</summary>
    let unknown = { Value = -1 }

    /// <summary>Wraps an integer as a source identifier without validating or registering it.</summary>
    let create value = { Value = value }

    /// <summary>Reports whether the source identifier is the unknown-source sentinel.</summary>
    let isUnknown source = source = unknown

type ReaderError = { Pos: int; Msg: string }

type IByteReader =

    /// <summary>Gets the complete buffer exposed by this reader without advancing it.</summary>
    abstract Buffer: ReadOnlyMemory<byte>
    /// <summary>Gets the current absolute byte position, including the reader's initial offset.</summary>
    abstract Position: int
    /// <summary>Gets the number of bytes still available to read.</summary>
    abstract Remaining: int

    /// <summary>Reads the requested bytes and advances on success; returns a reader error when the count is invalid.</summary>
    abstract Read: count: int -> Result<ReadOnlyMemory<byte>, ReaderError>

    /// <summary>Returns the requested bytes without advancing; returns a reader error when the count is invalid.</summary>
    abstract Peek: count: int -> Result<ReadOnlyMemory<byte>, ReaderError>

    /// <summary>Creates a reader for the requested upcoming bytes at the current absolute position without advancing this reader.</summary>
    abstract Slice: count: int -> Result<IByteReader, ReaderError>

    /// <summary>Advances by the requested byte count on success; returns a reader error when the count is invalid.</summary>
    abstract Skip: count: int -> Result<unit, ReaderError>

type ReaderFactory = ReadOnlyMemory<byte> -> int -> IByteReader

type ParserError = {
    Source: SourceId
    Pos: int
    Msg: string
}

module ParserError =

    /// <summary>Assigns the supplied source only when the parser error currently has an unknown source.</summary>
    let withDefaultSource source error =
        if error.Source |> SourceId.isUnknown then
            { error with Source = source }
        else
            error

type FieldId = private FieldId of int

module FieldId =

    /// <summary>Wraps an integer as a field identifier without validating or registering it.</summary>
    let create id = FieldId id

    /// <summary>Returns the integer represented by the field identifier.</summary>
    let value (FieldId id) = id

type SourceSpan = {
    Source: SourceId
    Offset: int
    Length: int
}

type SourceTransform =
    | Root
    | Decrypt of algorithm: string
    | Decompress of algorithm: string

type SourceInfo = {
    Id: SourceId
    Name: string
    Origin: SourceSpan option
    Transform: SourceTransform
    Length: int
    Sensitive: bool
}

type ISourceStore =

    /// <summary>Registers root bytes with a name and sensitivity flag and returns their source metadata.</summary>
    abstract AddRoot: name: string -> bytes: ReadOnlyMemory<byte> -> sensitive: bool -> SourceInfo

    /// <summary>Registers derived bytes with their origin, transformation and sensitivity and returns their source metadata.</summary>
    abstract AddDerived:
        name: string ->
        origin: SourceSpan ->
        transform: SourceTransform ->
        bytes: ReadOnlyMemory<byte> ->
        sensitive: bool ->
            SourceInfo

    /// <summary>Creates a reader positioned at the start of a previously registered source.</summary>
    abstract CreateReader: source: SourceId -> IByteReader

type IFieldTracer =

    /// <summary>Records the creation of a source and its metadata.</summary>
    abstract SourceCreated: source: SourceInfo -> unit

    /// <summary>Begins tracing a named field at its initial source span and returns its field identifier.</summary>
    abstract BeginField: name: string * start: SourceSpan -> FieldId

    /// <summary>Records successful completion of a field with its final source span.</summary>
    abstract EndField: id: FieldId * span: SourceSpan -> unit

    /// <summary>Records a parser failure associated with a previously begun field.</summary>
    abstract FailField: id: FieldId * error: ParserError -> unit
