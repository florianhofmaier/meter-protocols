using Microsoft.FSharp.Core;
using Metering.Common.Decoding.Parsers.Types;

namespace Metering.Common.Decoding.ByteReaders;

public class ByteReader(ReadOnlyMemory<byte> buffer, int offset) : IByteReader
{
    private int _position;

    public ReadOnlyMemory<byte> Buffer => buffer;

    public int Position => offset + _position;

    public int Remaining => buffer.Length - _position;

    private ReaderError? ValidateCount(int count)
    {
        if ((uint) count > (uint) Remaining)
            return new ReaderError(
                Position,
                $"Unexpected end of buffer. Requested {count} byte(s), remaining {Remaining}.");

        return null;
    }

    public FSharpResult<ReadOnlyMemory<byte>, ReaderError> Read(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewError(error);

        var slice = buffer.Slice(_position, count);
        _position += count;

        return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewOk(slice);
    }

    public FSharpResult<ReadOnlyMemory<byte>, ReaderError> Peek(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewError(error);

        return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewOk(
            buffer.Slice(_position, count));
    }

    public FSharpResult<IByteReader, ReaderError> Slice(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<IByteReader, ReaderError>.NewError(error);

        return FSharpResult<IByteReader, ReaderError>.NewOk(
            new ByteReader(buffer.Slice(_position, count), Position));
    }

    public FSharpResult<Unit, ReaderError> Skip(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<Unit, ReaderError>.NewError(error);

        _position += count;
        return FSharpResult<Unit, ReaderError>.NewOk(null!);
    }
}
