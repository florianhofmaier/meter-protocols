namespace Metering.Common.Decoding.Parsers.Types

open System

[<Struct>]
type SourceId =
    {
        Value : int
    }

module SourceId =

    let root =
        {
            Value = 0
        }

    let unknown =
        {
            Value = -1
        }

    let create value =
        {
            Value = value
        }

    let isUnknown source =
        source = unknown

type IByteReader =

    abstract Buffer : ReadOnlyMemory<byte>
    abstract Position : int
    abstract Remaining : int

    abstract Read:
        count: int -> ReadOnlyMemory<byte>

    abstract Peek:
        count: int -> ReadOnlyMemory<byte>

    abstract Slice:
        count: int -> IByteReader

    abstract Skip:
        count: int -> unit

type ByteReaderFactory =
    ReadOnlyMemory<byte> -> IByteReader

type ParserError =
    {
        Source : SourceId
        Pos : int
        Msg : string
    }

type ParserException(error: ParserError) =
    inherit Exception(error.Msg)

    member _.Error = error

module ParserError =

    let withDefaultSource source error =
        if error.Source |> SourceId.isUnknown then
            {
                error with
                    Source = source
            }
        else
            error

type FieldId =
    private FieldId of int

module FieldId =

    let create id = FieldId id

type SourceSpan =
    {
        Source : SourceId
        Offset : int
        Length : int
    }

type SourceTransform =
    | Root
    | Decrypt of algorithm: string
    | Decompress of algorithm: string

type SourceInfo =
    {
        Id : SourceId
        Name : string
        Origin : SourceSpan option
        Transform : SourceTransform
        Length : int
        Sensitive : bool
    }

type IFieldTracer =

    abstract SourceCreated :
        source: SourceInfo -> unit

    abstract BeginField :
        name: string * start: SourceSpan -> FieldId

    abstract EndField :
        id: FieldId * span: SourceSpan -> unit

    abstract FailField :
        id: FieldId * error: ParserError -> unit
