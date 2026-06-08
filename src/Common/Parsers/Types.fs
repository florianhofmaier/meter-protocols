namespace Metering.Common.Decoding.Parsers.Types

open System

type ParserError =
    {
        Pos : int
        Msg : string
    }

type ParserException(error: ParserError) =
    inherit Exception(error.Msg)

    member _.Error = error

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

type FieldId =
    private FieldId of int

module FieldId =

    let create id = FieldId id

type SourceSpan =
    {
        Offset : int
        Length : int
    }

type IFieldTracer =

    abstract BeginField :
        name: string * offset: int -> FieldId

    abstract EndField :
        id: FieldId * span: SourceSpan -> unit

    abstract FailField :
        id: FieldId * error: ParserError -> unit